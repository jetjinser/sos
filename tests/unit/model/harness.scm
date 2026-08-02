;;; tests/unit/model/harness.scm
;;; Shared helpers and test inputs for the scope-set model test suites.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (tests unit model harness)
  #:use-module (core-model)
  #:use-module (ssv source)
  #:export (term=?
            input-simple-macro
            input-reftrans
            input-hyg
            input-thunk
            input-get-identity
            input-prune
            input-gen
            input-local-value
            input-local-expand
            input-local-expand-stop
            input-local-binder
            input-box
            input-set-box
            input-defs-shadow
            input-defs-local-macro))

;;; Structural equality over stx objects and plain terms (spans ignored).
(define (term=? a b)
  (cond
   ((and (stx? a) (stx? b))
    (and (term=? (stx-form a) (stx-form b))
         (equal? (stx-ctx a) (stx-ctx b))))
   ((and (pair? a) (pair? b))
    (and (term=? (car a) (car b))
         (term=? (cdr a) (cdr b))))
   (else (equal? a b))))

;;; ----------------------------------------
;;; Test inputs (shared across models)

(define input-simple-macro
  (string->stx "(let-syntax x (lambda z (syntax (quote 2))) (x 1))"))

(define input-reftrans
  (string->stx "(lambda z (let-syntax x (lambda s (syntax z)) (lambda z (x))))"))

(define input-hyg
  (string->stx "(lambda z (let-syntax x (lambda s (datum->syntax (syntax here) (LIST (syntax lambda) (syntax z) (CAR (CDR (syntax->datum s)))))) (x z)))"))

(define input-thunk
  (string->stx "(let-syntax thunk (lambda e (datum->syntax e (LIST (syntax lambda) (syntax a) (CAR (CDR (syntax->datum e)))))) (((lambda a (thunk (+ a 1))) 5) 0))"))

(define input-get-identity
  (string->stx "(let-syntax get-identity (lambda e (datum->syntax e (LIST (syntax lambda) (syntax a) (datum->syntax e (LIST (syntax lambda) (CAR (CDR (syntax->datum e))) (syntax a)))))) (get-identity a))"))

(define input-prune
  (string->stx "(let-syntax x (lambda e ((lambda id1 ((lambda id2 (datum->syntax e (LIST (syntax let-syntax) (syntax f) (datum->syntax e (LIST (syntax lambda) id2 (datum->syntax e (LIST (syntax CAR) (datum->syntax e (LIST (syntax CDR) (datum->syntax e (LIST (syntax syntax->datum) id1)))))))) (syntax (f 3))))) (syntax y))) (syntax y))) (x))"))

(define input-gen
  (string->stx "(let-syntax x (lambda e ((lambda id1 ((lambda id2 (datum->syntax e (LIST (syntax lambda) id2 id1))) (syntax y))) (syntax y))) (x))"))

(define input-local-value
  (string->stx "(let-syntax a 8 (let-syntax b 9 (let-syntax x (lambda s (datum->syntax (syntax here) (LIST (syntax quote) (datum->syntax (syntax here) (LOCAL-VALUE (CAR (CDR (syntax->datum s)))))))) (x a))))"))

(define input-local-expand
  (string->stx "(let-syntax q (lambda s (syntax (CAR 8))) (let-syntax x (lambda s (CAR (CDR (syntax->datum (LOCAL-EXPAND (CAR (CDR (syntax->datum s))) (LIST)))))) (x (q))))"))

(define input-local-expand-stop
  (string->stx "(let-syntax p (lambda s (quote 0)) (let-syntax q (lambda s (syntax (CAR 8))) (let-syntax x (lambda s (CAR (CDR (syntax->datum (LOCAL-EXPAND (CAR (CDR (syntax->datum s))) (LIST (syntax p))))))) (x (q)))))"))

(define input-local-binder
  (string->stx "(let-syntax q (lambda e (datum->syntax e (LIST (syntax quote) (CAR (CDR (syntax->datum e)))))) (let-syntax a (lambda e (datum->syntax e (LIST (syntax lambda) (LOCAL-BINDER (CAR (CDR (syntax->datum (LOCAL-EXPAND (CAR (CDR (syntax->datum e))) (LIST)))))) (CAR (CDR (CDR (syntax->datum e))))))) (a (q x) x)))"))

(define input-box
  (string->stx "(let-syntax m (lambda e (datum->syntax e (LIST (syntax quote) (datum->syntax e ((lambda b (UNBOX b)) (BOX 0)))))) (m))"))

(define input-set-box
  (string->stx "(let-syntax m (lambda e (datum->syntax e (LIST (syntax quote) (datum->syntax e ((lambda b ((lambda x (UNBOX b)) (SET-BOX! b 1))) (BOX 0)))))) (m))"))

(define input-defs-shadow
  (string->stx "(let-syntax call (lambda s (datum->syntax (syntax here) (LIST (CAR (CDR (syntax->datum s)))))) (let-syntax p (lambda s (syntax 0)) (let-syntax q (lambda s ((lambda defs ((lambda ignored (datum->syntax (syntax here) (LIST (syntax lambda) (LOCAL-BINDER (CAR (CDR (syntax->datum (LOCAL-EXPAND (datum->syntax (syntax here) (LIST (syntax quote) (CAR (CDR (syntax->datum s))))) (LIST) defs))))) (LOCAL-EXPAND (CAR (CDR (CDR (syntax->datum s)))) (LIST (syntax call)) defs)))) (DEF-BIND defs (CAR (CDR (syntax->datum s)))))) (NEW-DEFS))) (q p (call p)))))"))

(define input-defs-local-macro
  (string->stx "(let-syntax call (lambda s (datum->syntax (syntax here) (LIST (CAR (CDR (syntax->datum s)))))) (let-syntax p (lambda s (syntax 0)) (let-syntax q (lambda s ((lambda defs ((lambda ignored (datum->syntax (syntax here) (LIST (syntax lambda) (CAR (CDR (syntax->datum (LOCAL-EXPAND (datum->syntax (syntax here) (LIST (syntax quote) (CAR (CDR (syntax->datum s))))) (LIST) defs)))) (LOCAL-EXPAND (CAR (CDR (CDR (syntax->datum s)))) (LIST) defs)))) (DEF-BIND defs (CAR (CDR (syntax->datum s))) (datum->syntax (syntax here) (LIST (syntax lambda) (syntax s) (CAR (CDR (CDR (CDR (syntax->datum s)))))))))) (NEW-DEFS))) (q p (call p) (syntax 13)))))"))

