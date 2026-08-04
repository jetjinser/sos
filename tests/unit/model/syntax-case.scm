;;; tests/unit/model/syntax-case.scm
;;; Test syntax-case and the syntax template as eval-level primitives: pattern
;;; matching with literals and a trailing ellipsis, template instantiation by
;;; pattern-variable substitution, and hygiene, propagated to all four models.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (tests unit model syntax-case)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 receive)
  #:use-module (core-model)
  #:use-module (phases-model)
  #:use-module (local-model)
  #:use-module (defs-model)
  #:use-module (ssv source)
  #:use-module (tests unit model harness))

(test-begin "model-syntax-case")

(define (run src)
  (receive (expanded store) (expand (string->stx src) (primitives-env) (init-store))
    (parse expanded store)))

(define (run-phases src)
  (receive (e s) (ph-expand 0 (string->stx src) (primitives-env) '() (init-store))
    (ph-parse 0 e s)))

(define (run-local src)
  (receive (e s*) (loc-expand 0 (string->stx src) (primitives-env)
                              (list (init-store) '() '()))
    (ph-parse 0 e (car s*))))

(define (run-defs src)
  (receive (e s*) (defs-expand 0 (string->stx src) (primitives-env)
                               (list (init-store) '() '()))
    (ph-parse 0 e (car s*))))

(define my-let-src
  "(let-syntax my-let (lambda use (syntax-case use () ((_ (x v) body) (syntax ((lambda x body) v))))) (my-let (a 5) a))")

(test-assert "my-let"
  (term=? (run my-let-src)
          '(app (fun (var a:6) (var a:6)) 5)))

(test-assert "twice"
  (term=? (run "(let-syntax twice (lambda use (syntax-case use () ((_ e) (syntax (CONS e e))))) (twice 3))")
          '(app CONS 3 3)))

(test-assert "literal-match"
  (term=? (run "(let-syntax g (lambda use (syntax-case use (kw) ((_ kw x) (syntax (LIST kw x))) ((_ x) (syntax (LIST x))))) (g kw 5))")
          '(app LIST (var kw) 5)))

(test-assert "literal-fallthrough"
  (term=? (run "(let-syntax g (lambda use (syntax-case use (kw) ((_ kw x) (syntax (LIST kw x))) ((_ x) (syntax (LIST x))))) (g 5))")
          '(app LIST 5)))

(test-assert "ellipsis-splice"
  (term=? (run "(let-syntax my-list (lambda use (syntax-case use () ((_ x ...) (syntax (LIST x ...))))) (my-list 1 2 3))")
          '(app LIST 1 2 3)))

(test-assert "ellipsis-empty"
  (term=? (run "(let-syntax my-list (lambda use (syntax-case use () ((_ x ...) (syntax (LIST x ...))))) (my-list))")
          '(app LIST)))

;;; Unlike syntax-rules, the clause body is arbitrary transformer code: a
;;; pattern variable can be used as a value directly.
(test-assert "clause-body-expression"
  (term=? (run "(let-syntax first (lambda use (syntax-case use () ((_ a b) a))) (first 7 8))")
          7))

;;; Introduced identifiers bind each other but not the use site.
(test-assert "introduced-binder"
  (term=? (run "(let-syntax intro (lambda use (syntax-case use () ((_ e) (syntax ((lambda t t) e))))) (intro 5))")
          '(app (fun (var t:6) (var t:6)) 5)))

;;; Propagated to the phase-aware models.
(test-assert "phases-my-let"
  (term=? (run-phases my-let-src) '(app (fun (var a:6) (var a:6)) 5)))

(test-assert "local-my-let"
  (term=? (run-local my-let-src) '(app (fun (var a:6) (var a:6)) 5)))

(test-assert "defs-my-let"
  (term=? (run-defs my-let-src) '(app (fun (var a:6) (var a:6)) 5)))

(test-end "model-syntax-case")
