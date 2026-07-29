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
  (string->stx (string-append
                "(lambda z (let-syntax x (lambda s"
                " (MKS (LIST (syntax lambda) (syntax z) (CAR (CDR (SE s))))"
                " (syntax here))) (x z)))")))

(define input-thunk
  (string->stx (string-append
                "(let-syntax thunk (lambda e"
                " (MKS (LIST (syntax lambda) (syntax a) (CAR (CDR (SE e)))) e))"
                " (((lambda a (thunk (+ a 1))) 5) 0))")))

(define input-get-identity
  (string->stx (string-append
                "(let-syntax get-identity (lambda e"
                " (MKS (LIST (syntax lambda) (syntax a)"
                " (MKS (LIST (syntax lambda) (CAR (CDR (SE e))) (syntax a)) e)) e))"
                " (get-identity a))")))

(define input-prune
  (string->stx (string-append
                "(let-syntax x (lambda e"
                " ((lambda id1"
                " ((lambda id2"
                " (MKS (LIST (syntax let-syntax) (syntax f)"
                " (MKS (LIST (syntax lambda) id2"
                " (MKS (LIST (syntax CAR)"
                " (MKS (LIST (syntax CDR)"
                " (MKS (LIST (syntax SE) id1) e)) e)) e)) e)"
                " (syntax (f 3))) e))"
                " (syntax y))) (syntax y))) (x))")))

(define input-gen
  (string->stx (string-append
                "(let-syntax x (lambda e"
                " ((lambda id1"
                " ((lambda id2"
                " (MKS (LIST (syntax lambda) id2 id1) e))"
                " (syntax y))) (syntax y))) (x))")))

(define input-local-value
  (string->stx (string-append
                "(let-syntax a 8"
                " (let-syntax b 9"
                " (let-syntax x (lambda s"
                " (MKS (LIST (syntax quote)"
                " (MKS (LOCAL-VALUE (CAR (CDR (SE s)))) (syntax here)))"
                " (syntax here))) (x a))))")))

(define input-local-expand
  (string->stx (string-append
                "(let-syntax q (lambda s (syntax (CAR 8)))"
                " (let-syntax x (lambda s"
                " (CAR (CDR (SE (LOCAL-EXPAND (CAR (CDR (SE s))) (LIST))))))"
                " (x (q))))")))

(define input-local-expand-stop
  (string->stx (string-append
                "(let-syntax p (lambda s (quote 0))"
                " (let-syntax q (lambda s (syntax (CAR 8)))"
                " (let-syntax x (lambda s"
                " (CAR (CDR (SE (LOCAL-EXPAND (CAR (CDR (SE s)))"
                " (LIST (syntax p)))))))"
                " (x (q)))))")))

(define input-local-binder
  (string->stx (string-append
                "(let-syntax q (lambda e"
                " (MKS (LIST (syntax quote) (CAR (CDR (SE e)))) e))"
                " (let-syntax a (lambda e"
                " (MKS (LIST (syntax lambda)"
                " (LOCAL-BINDER"
                " (CAR (CDR (SE (LOCAL-EXPAND (CAR (CDR (SE e))) (LIST))))))"
                " (CAR (CDR (CDR (SE e))))) e))"
                " (a (q x) x)))")))

(define input-box
  (string->stx (string-append
                "(let-syntax m (lambda e"
                " (MKS (LIST (syntax quote)"
                " (MKS ((lambda b (UNBOX b)) (BOX 0)) e)) e)) (m))")))

(define input-set-box
  (string->stx (string-append
                "(let-syntax m (lambda e"
                " (MKS (LIST (syntax quote)"
                " (MKS ((lambda b ((lambda x (UNBOX b)) (SET-BOX! b 1)))"
                " (BOX 0)) e)) e)) (m))")))

(define input-defs-shadow
  (string->stx (string-append
                "(let-syntax call (lambda s"
                " (MKS (LIST (CAR (CDR (SE s)))) (syntax here)))"
                " (let-syntax p (lambda s (syntax 0))"
                " (let-syntax q (lambda s"
                " ((lambda defs"
                " ((lambda ignored"
                " (MKS (LIST (syntax lambda)"
                " (LOCAL-BINDER"
                " (CAR (CDR (SE (LOCAL-EXPAND"
                " (MKS (LIST (syntax quote) (CAR (CDR (SE s)))) (syntax here))"
                " (LIST) defs)))))"
                " (LOCAL-EXPAND (CAR (CDR (CDR (SE s))))"
                " (LIST (syntax call)) defs))"
                " (syntax here)))"
                " (DEF-BIND defs (CAR (CDR (SE s))))))"
                " (NEW-DEFS)))"
                " (q p (call p)))))")))

(define input-defs-local-macro
  (string->stx (string-append
                "(let-syntax call (lambda s"
                " (MKS (LIST (CAR (CDR (SE s)))) (syntax here)))"
                " (let-syntax p (lambda s (syntax 0))"
                " (let-syntax q (lambda s"
                " ((lambda defs"
                " ((lambda ignored"
                " (MKS (LIST (syntax lambda)"
                " (CAR (CDR (SE (LOCAL-EXPAND"
                " (MKS (LIST (syntax quote) (CAR (CDR (SE s)))) (syntax here))"
                " (LIST) defs))))"
                " (LOCAL-EXPAND (CAR (CDR (CDR (SE s)))) (LIST) defs))"
                " (syntax here)))"
                " (DEF-BIND defs (CAR (CDR (SE s)))"
                " (MKS (LIST (syntax lambda) (syntax s)"
                " (CAR (CDR (CDR (CDR (SE s)))))) (syntax here)))))"
                " (NEW-DEFS)))"
                " (q p (call p) (syntax 13)))))")))
