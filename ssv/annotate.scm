;;; ssv/annotate.scm
;;; Project a recorded trace into source-span annotations the frontend renders
;;; directly: per-step scope-set snapshots (tinting) and final resolve names
;;; (inlay chips).  Pure post-processing — the models are untouched, and the
;;; frontend stays a dumb renderer.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (ssv annotate)
  #:use-module (srfi srfi-1)
  #:use-module (core-model)
  #:export (stx-span-alist
            step-snapshots
            resolve-alist
            binder-alist
            use-alist
            step-stores
            trace-snapshots
            trace-resolve
            trace-binders
            trace-uses
            trace-stores))

;;; ----------------------------------------
;;; Flatten a stx tree to (span . ctx) entries

(define (stx-span-alist stx)
  (span-ctx-into stx '()))

(define (span-ctx-into stx acc)
  (if (not (stx? stx))
      acc
      (let ((form (stx-form stx))
            (acc  (if (stx-span stx)
                      (cons (cons (stx-span stx) (stx-ctx stx)) acc)
                      acc)))
        (if (pair? form)
            (let loop ((fs form) (acc acc))
              (if (null? fs)
                  acc
                  (loop (cdr fs) (span-ctx-into (car fs) acc))))
            acc))))

;;; ----------------------------------------
;;; Ordering (deterministic output, keyed by span)

(define (span<? a b)
  (let ((sa (car a)) (sb (car b)))
    (or (< (car sa) (car sb))
        (and (= (car sa) (car sb))
             (< (cdr sa) (cdr sb))))))

(define (sort-by-span entries)
  (sort entries span<?))

;;; ----------------------------------------
;;; Per-step scope snapshots

;;; Overlay NEW onto STATE, replacing entries with the same span.
(define (merge new state)
  (let loop ((new new) (state state))
    (if (null? new)
        state
        (let ((span (caar new)))
          (loop (cdr new)
                (cons (car new)
                      (filter (lambda (e) (not (equal? (car e) span)))
                              state)))))))

(define (nonempty-ctx e)
  (not (null? (cdr e))))

;;; The stx a record leaves behind: an op's result, or a rule's rebuilt form.
(define (record-after rec)
  (case (car rec)
    [(op)   (and (memq (cadr rec) '(stx-add stx-flip stx-prune))
                 (list-ref rec 4))]
    [(rule) (cadddr rec)]
    [else   #f]))

(define (apply-record rec state)
  (let ((after (record-after rec)))
    (if after
        (filter nonempty-ctx
                (sort-by-span (merge (span-ctx-into after '()) state)))
        state)))

;;; One snapshot per record: the cumulative span -> ctx (non-empty only) after
;;; applying records up to and including that step.
(define (step-snapshots input-stx records)
  (let loop ((records records)
             (state   (filter nonempty-ctx (stx-span-alist input-stx)))
             (out     '()))
    (if (null? records)
        (reverse out)
        (let ((state (apply-record (car records) state)))
          (loop (cdr records) state (cons state out))))))

;;; ----------------------------------------
;;; Per-step store states

;;; The counter value encoded in a fresh name like z:3.
(define (name-counter name)
  (let loop ((cs (reverse (string->list (symbol->string name))))
             (digits '()))
    (cond
     [(null? cs) 0]
     [(char=? (car cs) #\:)
      (if (null? digits) 0 (string->number (list->string digits)))]
     [else (loop (cdr cs) (cons (car cs) digits))])))

(define (add-bind binds sym scopes name)
  (let ((existing (assq sym binds)))
    (if existing
        (map (lambda (b)
               (if (eq? (car b) sym)
                   (cons sym (cons (cons scopes name) (cdr b)))
                   b))
             binds)
        (cons (list sym (cons scopes name)) binds))))

;;; Rule records carry the authoritative store; store-mutating ops (bind,
;;; alloc-name, alloc-scope) are applied incrementally on top of it.
(define (apply-record-store rec store)
  (case (car rec)
    [(rule) (list-ref rec 4)]
    [(op)
     (let ((kind (cadr rec))
           (data (cddr rec)))
       (cond
        [(memq kind '(alloc-name alloc-scope))
         (make-store (max (store-counter store)
                          (+ (name-counter (car data)) 1))
                     (store-binds store)
                     (store-boxes store)
                     (store-def-envs store))]
        [(eq? kind 'bind)
         (make-store (store-counter store)
                     (add-bind (store-binds store)
                               (list-ref data 0)
                               (list-ref data 1)
                               (list-ref data 2))
                     (store-boxes store)
                     (store-def-envs store))]
        [else store]))]
    [else store]))

(define (step-stores records)
  (let loop ((records records)
             (store   (init-store))
             (out     '()))
    (if (null? records)
        (reverse out)
        (let ((store (apply-record-store (car records) store)))
          (loop (cdr records) store (cons store out))))))

;;; ----------------------------------------
;;; Resolve names for source identifiers (final state)

;;; A source identifier warrants a chip only when it actually resolves to a
;;; binding (fresh name); keywords, primitives and free variables resolve to
;;; themselves and are skipped.
(define (resolve-into stx store resolve-proc acc)
  (if (not (stx? stx))
      acc
      (let ((form (stx-form stx))
            (span (stx-span stx)))
        (cond
         [(pair? form)
          (let loop ((fs form) (acc acc))
            (if (null? fs)
                acc
                (loop (cdr fs) (resolve-into (car fs) store resolve-proc acc))))]
         [(and span (symbol? form))
          (let ((resolved (resolve-proc stx store)))
            (if (eq? resolved form)
                acc
                (cons (cons span resolved) acc)))]
         [else acc]))))

(define (resolve-alist stx store resolve-proc)
  (sort-by-span (resolve-into stx store resolve-proc '())))

;;; ----------------------------------------
;;; Binder positions (where each fresh name was introduced)

;;; The bind op carries the binder's source span as its 4th datum; collect
;;; (span . name) entries so the frontend can link every use back to the
;;; identifier that bound it.  Binders with no source span (macro-generated)
;;; are skipped — there is nothing to point at.
(define (binder-into records acc)
  (if (null? records)
      acc
      (let ((rec (car records)))
        (binder-into
         (cdr records)
         (if (and (eq? (car rec) 'op)
                  (eq? (cadr rec) 'bind)
                  (>= (length rec) 6)
                  (pair? (list-ref rec 5)))
             (cons (cons (list-ref rec 5) (list-ref rec 4)) acc)
             acc)))))

(define (binder-alist records)
  (sort-by-span (binder-into records '())))

;;; ----------------------------------------
;;; Use sites (where bound identifiers are referenced)

;;; Two kinds of reference reach a binder, both collected as (use-span .
;;; binder-span):
;;;  - variable use: the id rule fires on a reference to a bound variable; its
;;;    before-stx is the use site and its tvar info carries the binder's stx,
;;;    whose span identifies the binder (no resolution needed);
;;;  - macro use: a macro-invoke's operator identifier refers to the macro's
;;;    let-syntax binder; resolve it to a name and look the name up in the
;;;    binder table.  resolve-proc keeps this phase-correct across models.

;;; name -> binder span, inverted from the bind ops.
(define (binder-name-index records)
  (map (lambda (e) (cons (cdr e) (car e))) (binder-alist records)))

(define (use-entry rec resolve-proc name-index)
  (cond
   [(and (eq? (car rec) 'rule) (eq? (cadr rec) 'id))
    (let ((tv (assq 'tvar (list-ref rec 6))))
      (and tv
           (let ((use-span    (stx-span (caddr rec)))
                 (binder-span (stx-span (cdr tv))))
             (and use-span binder-span (cons use-span binder-span)))))]
   [(and (eq? (car rec) 'rule) (eq? (cadr rec) 'macro-invoke))
    (let* ((form  (stx-form (caddr rec)))
           (first (and (pair? form) (car form))))
      (and (stx? first) (stx-span first)
           (let ((binder (assq (resolve-proc first (list-ref rec 4)) name-index)))
             (and binder (cons (stx-span first) (cdr binder))))))]
   [else #f]))

(define (use-into records resolve-proc name-index acc)
  (if (null? records)
      acc
      (let ((entry (use-entry (car records) resolve-proc name-index)))
        (use-into (cdr records) resolve-proc name-index
                  (if entry (cons entry acc) acc)))))

;;; Sorted, with duplicates (a use re-expanded more than once) collapsed.
(define (use-alist records resolve-proc)
  (let ((name-index (binder-name-index records)))
    (let loop ((entries (sort-by-span (use-into records resolve-proc name-index '())))
               (out '()))
      (cond
       [(null? entries) (reverse out)]
       [(and (pair? out) (equal? (car entries) (car out)))
        (loop (cdr entries) out)]
       [else (loop (cdr entries) (cons (car entries) out))]))))

;;; ----------------------------------------
;;; Convenience: operate on a run-traced result alist

(define (trace-snapshots trace)
  (step-snapshots (cdr (assq 'input trace))
                  (cdr (assq 'steps trace))))

(define (trace-resolve trace resolve-proc)
  (resolve-alist (cdr (assq 'final-stx trace))
                 (cdr (assq 'final-store trace))
                 resolve-proc))

(define (trace-binders trace)
  (binder-alist (cdr (assq 'steps trace))))

(define (trace-uses trace resolve-proc)
  (use-alist (cdr (assq 'steps trace)) resolve-proc))

(define (trace-stores trace)
  (step-stores (cdr (assq 'steps trace))))
