;;; tests/unit/model/defs.scm
;;; Test suite for the definition-contexts model (defs-model).
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (tests unit model defs)
  #:use-module (srfi srfi-64)
  #:use-module (core-model)
  #:use-module (phases-model)
  #:use-module (defs-model)
  #:use-module (tests unit model harness))

(test-begin "model-defs")

;;; Σ* = (store scps-p scps-u); the store is the cadr of the result pair.
(define (run input)
  (let ((er (defs-expand 0 (as-syntax input) (primitives-env)
                         (list (init-store) '() '()))))
    (ph-parse 0 (car er) (cadr er))))

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
(test-assert "box"
  (term=? (run input-box) 0))
(test-assert "set-box"
  (term=? (run input-set-box) 1))
(test-assert "defs-shadow"
  (term=? (run input-defs-shadow) '(fun (var p:23) (app (var p:23)))))
(test-assert "defs-local-macro"
  (term=? (run input-defs-local-macro) '(fun (var p:27) 13)))

(test-end "model-defs")
