;;; tests/unit/model/core.scm
;;; Test suite for the single-phase scope-set model (core-model).
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (tests unit model core)
  #:use-module (srfi srfi-64)
  #:use-module (core-model)
  #:use-module (tests unit model harness))

(test-begin "model-core")

(define (run input)
  (let ((er (expand (as-syntax input) (primitives-env) (init-store))))
    (parse (car er) (cdr er))))

(test-assert "simple-macro"
  (term=? (run input-simple-macro) 2))
(test-assert "reftrans-macro"
  (term=? (run input-reftrans) '(fun (var z:0) (fun (var z:4) (var z:0)))))
(test-assert "hyg-macro"
  (term=? (run input-hyg) '(fun (var z:0) (fun (var z:6) (var z:0)))))
(test-assert "thunk"
  (term=? (run input-thunk)
          '(app (app (fun (var a:2) (fun (var a:6) (app + (var a:2) 1))) 5) 0)))
(test-assert "get-identity"
  (term=? (run input-get-identity) '(fun (var a:4) (fun (var a:6) (var a:4)))))

(test-end "model-core")
