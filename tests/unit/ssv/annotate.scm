;;; tests/unit/ssv/annotate.scm
;;; Test the trace-to-annotation projection: span flattening, per-step scope
;;; snapshots, and final resolve names.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (tests unit ssv annotate)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (core-model)
  #:use-module (phases-model)
  #:use-module (defs-model)
  #:use-module (ssv source)
  #:use-module (ssv trace)
  #:use-module (ssv annotate)
  #:use-module (tests unit model harness))

(test-begin "ssv-annotate")

;;; ----------------------------------------
;;; Helpers

(define (sort-spans entries)
  (sort entries (lambda (a b)
                  (or (< (caar a) (caar b))
                      (and (= (caar a) (caar b))
                           (< (cdar a) (cdar b)))))))

(define (ast-vars ast)
  (match ast
    [('var name) (list name)]
    [('fun ('var bvar) body) (cons bvar (ast-vars body))]
    [('app . args) (append-map ast-vars args)]
    [('list-val . elts) (append-map ast-vars elts)]
    [_ '()]))

(define (count-eq x lst)
  (length (filter (cut equal? <> x) lst)))

(define (submultiset? small big)
  (every (lambda (x) (<= (count-eq x small) (count-eq x big)))
         (delete-duplicates small)))

;;; ----------------------------------------
;;; stx-span-alist: exact spans for a parsed source

(test-equal "span-alist-atom"
  '(((0 . 1) . ()))
  (stx-span-alist (string->stx "x")))

(test-equal "span-alist-compound"
  '(((0 . 5) . ()) ((1 . 2) . ()) ((3 . 4) . ()))
  (sort-spans (stx-span-alist (string->stx "(a b)"))))

;;; ----------------------------------------
;;; step-snapshots: one per step, sorted, non-empty ctx, converges on survivors

(define lambda-zz-trace
  (run-traced 'core (string->stx "(lambda z z)")))

(define lambda-zz-snaps
  (trace-snapshots lambda-zz-trace))

(test-equal "snapshots-count-matches-steps"
  (length (cdr (assq 'steps lambda-zz-trace)))
  (length lambda-zz-snaps))

(test-assert "snapshots-sorted-and-nonempty"
  (every (lambda (snap)
           (and (equal? snap (sort-spans snap))
                (every (lambda (e) (not (null? (cdr e)))) snap)))
         lambda-zz-snaps))

;;; The final snapshot must cover every surviving source span's final ctx.
(test-assert "snapshots-cover-final-stx"
  (let ((last (last lambda-zz-snaps))
        (final (filter (lambda (e) (not (null? (cdr e))))
                       (stx-span-alist (cdr (assq 'final-stx lambda-zz-trace))))))
    (every (lambda (e)
             (member e last (lambda (x y)
                              (and (equal? (car x) (car y))
                                   (equal? (cdr x) (cdr y))))))
           (delete-duplicates final))))

;;; (lambda z z): binder z at span (8 . 9) gains the lambda's fresh scope.
(test-assert "snapshot-binder-gains-scope"
  (let ((last (last lambda-zz-snaps)))
    (any (lambda (e)
           (and (equal? (car e) '(8 . 9))
                (= (length (cdr e)) 1)))
         last)))

;;; ----------------------------------------
;;; resolve-alist: source identifiers map to their resolved names

(test-equal "resolve-lambda-zz"
  '(((8 . 9) . z:0) ((8 . 9) . z:0))
  (trace-resolve lambda-zz-trace resolve))

;;; Resolved names are a subset of the ast's variables (generated identifiers
;;; carry no span, so they are never annotated).
(test-assert "resolve-subset-of-ast-vars"
  (let* ((tr (run-traced 'core (string->stx
                                "(lambda z (let-syntax x (lambda s (syntax z)) (lambda z (x))))")))
         (names (map cdr (trace-resolve tr resolve))))
    (and (pair? names)
         (submultiset? names (ast-vars (cdr (assq 'final-ast tr)))))))

;;; resolve-proc injection: phases model resolves through phase 0.
(test-assert "resolve-phases-injection"
  (let* ((tr (run-traced 'phases (string->stx "(lambda z z)")))
          (names (map cdr (trace-resolve tr
                                          (lambda (id store)
                                            (ph-resolve 0 id store))))))
    (submultiset? names (ast-vars (cdr (assq 'final-ast tr))))))

;;; ----------------------------------------
;;; binder-alist / use-alist: binder<->use linkage

(test-equal "binder-lambda-zz"
  '(((8 . 9) . z:0))
  (trace-binders lambda-zz-trace))

;;; (lambda z (CONS z z)): both uses of z refer back to the binder at (8 . 9).
(test-equal "uses-lambda-cons"
  '(((16 . 17) . (8 . 9)) ((18 . 19) . (8 . 9)))
  (trace-uses (run-traced 'core (string->stx "(lambda z (CONS z z))")) resolve))

;;; A macro use links back to its let-syntax binder: in the simple-macro
;;; example the (x 1) invocation refers to the x bound by let-syntax.
(test-assert "uses-macro-invocation"
  (let* ((tr (run-traced 'core input-simple-macro))
         (binder-spans (map car (trace-binders tr))))
    (any (lambda (u) (member (cdr u) binder-spans equal?))
         (trace-uses tr resolve))))

;;; Every use's target span is an actual binder span, so the linkage is
;;; consistent across a real expansion.
(test-assert "uses-point-to-binders"
  (let* ((tr (run-traced 'core input-hyg))
         (binder-spans (map car (trace-binders tr))))
    (every (lambda (u) (member (cdr u) binder-spans equal?))
           (trace-uses tr resolve))))

;;; ----------------------------------------
;;; step-stores: replayed store agrees with every rule's authoritative store

(define (store=? a b)
  (and (= (store-counter a) (store-counter b))
       (equal? (store-binds a) (store-binds b))
       (equal? (store-boxes a) (store-boxes b))
       (equal? (store-def-envs a) (store-def-envs b))))

(define (rule-stores-agree model input)
  (let ((steps (cdr (assq 'steps (run-traced model input)))))
    (let ((stores (step-stores steps)))
      (and (= (length stores) (length steps))
           (every (lambda (rec st)
                    (if (eq? (car rec) 'rule)
                        (store=? st (list-ref rec 4))
                        #t))
                  steps stores)))))

(test-assert "stores-agree-core"
  (rule-stores-agree 'core input-hyg))

(test-assert "stores-agree-defs"
  (rule-stores-agree 'defs input-defs-shadow))

(test-assert "stores-final-agrees"
  (let ((tr (run-traced 'core input-hyg)))
    (store=? (last (step-stores (cdr (assq 'steps tr))))
             (cdr (assq 'final-store tr)))))

(test-end "ssv-annotate")
