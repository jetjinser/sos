;;; tests/unit/model/phases.scm
;;; Test suite for the multi-phase scope-set model (phases-model).
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (tests unit model phases)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 receive)
  #:use-module (core-model)
  #:use-module (phases-model)
  #:use-module (tests unit model harness))

(test-begin "model-phases")

(define (run input)
  (receive (expanded store) (ph-expand 0 input (primitives-env) '() (init-store))
    (ph-parse 0 expanded store)))

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

(test-end "model-phases")
