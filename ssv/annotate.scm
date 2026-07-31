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
            step-stores
            trace-snapshots
            trace-resolve
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
;;; Convenience: operate on a run-traced result alist

(define (trace-snapshots trace)
  (step-snapshots (cdr (assq 'input trace))
                  (cdr (assq 'steps trace))))

(define (trace-resolve trace resolve-proc)
  (resolve-alist (cdr (assq 'final-stx trace))
                 (cdr (assq 'final-store trace))
                 resolve-proc))

(define (trace-stores trace)
  (step-stores (cdr (assq 'steps trace))))
