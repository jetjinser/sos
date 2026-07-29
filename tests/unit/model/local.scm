;;; tests/unit/model/local.scm
;;; Test suite for the local-expansion model (local-model).
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (tests unit model local)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 receive)
  #:use-module (core-model)
  #:use-module (phases-model)
  #:use-module (local-model)
  #:use-module (tests unit model harness))

(test-begin "model-local")

(define (run input)
  (receive (expanded s*) (loc-expand 0 input (primitives-env)
                                     (list (init-store) '() '()))
    (ph-parse 0 expanded (car s*))))

(test-assert "simple-macro"
  (term=? (run input-simple-macro) 2))
(test-assert "reftrans-macro"
  (term=? (run input-reftrans) '(fun (var z:0) (fun (var z:6) (var z:0)))))
(test-assert "hyg-macro"
  (term=? (run input-hyg) '(fun (var z:0) (fun (var z:8) (var z:0)))))
(test-assert "thunk"
  (term=? (run input-thunk)
          '(app (app (fun (var a:4) (fun (var a:8) (app + (var a:4) 1))) 5) 0)))
(test-assert "get-identity"
  (term=? (run input-get-identity) '(fun (var a:6) (fun (var a:8) (var a:6)))))
(test-assert "prune"
  (term=? (run input-prune) 3))
(test-assert "gen"
  (term=? (run input-gen) '(fun (var y:10) (var y:10))))
(test-assert "local-value"
  (term=? (run input-local-value) 8))
(test-assert "local-expand"
  (term=? (run input-local-expand) 8))
(test-assert "local-expand-stop"
  (term=? (run input-local-expand-stop) 8))
(test-assert "local-binder"
  (term=? (run input-local-binder) '(fun (var x:12) (var x:12))))

(test-end "model-local")
