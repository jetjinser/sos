;;; tests/unit/model/syntax-rules.scm
;;; Test syntax-rules as a library macro built on the syntax API: the expander
;;; has no syntax-rules rule; it is a registered for-syntax transformer that
;;; compiles a matcher which deconstructs the use-site and rebuilds the template.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (tests unit model syntax-rules)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 receive)
  #:use-module (core-model)
  #:use-module (ssv source)
  #:use-module (ssv syntax-rules)
  #:use-module (tests unit model harness))

(test-begin "model-syntax-rules")

(define (run src)
  (receive (expanded store) (expand (string->stx src) (primitives-env) (init-store))
    (parse expanded store)))

(test-assert "my-let"
  (term=? (run "(let-syntax my-let (syntax-rules () ((_ (x v) body) ((lambda x body) v))) (my-let (a 5) a))")
          '(app (fun (var a:8) (var a:8)) 5)))

(test-assert "twice"
  (term=? (run "(let-syntax twice (syntax-rules () ((_ e) (CONS e e))) (twice 3))")
          '(app CONS 3 3)))

(test-assert "inc-literal"
  (term=? (run "(let-syntax inc (syntax-rules () ((_ n) (+ n 1))) (inc 4))")
          '(app + 4 1)))

(test-end "model-syntax-rules")
