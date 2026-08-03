;;; tests/unit/model/core.scm
;;; Test suite for the single-phase scope-set model (core-model).
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (tests unit model core)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 receive)
  #:use-module (core-model)
  #:use-module (tests unit model harness))

(test-begin "model-core")

(define (run input)
  (receive (expanded store) (expand input (primitives-env) (init-store))
    (parse expanded store)))

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

;;; ----------------------------------------
;;; Syntax API primitives

(test-assert "bound-identifier=?-same-ctx"
  (delta 'bound-identifier=?
         (list (make-stx 'x '(s1 s2) #f)
               (make-stx 'y '(s1 s2) #f))))

(test-assert "bound-identifier=?-diff-ctx"
  (not (delta 'bound-identifier=?
              (list (make-stx 'x '(s1) #f)
                    (make-stx 'x '(s2) #f)))))

(test-assert "free-identifier=?-same-resolution"
  (receive (result store)
      (eval-ast `(app free-identifier=? ,(make-stx 'x '(s1) #f) ,(make-stx 'x '(s2) #f))
                (init-store))
    result))

(test-assert "free-identifier=?-diff-resolution"
  (receive (result store)
      (eval-ast `(app free-identifier=? ,(make-stx 'x '() #f) ,(make-stx 'y '() #f))
                (init-store))
    (not result)))

(test-assert "generate-temporaries-fresh-scopes"
  (receive (result store)
      (eval-ast `(app generate-temporaries
                      (app LIST ,(make-stx 'x '() #f) ,(make-stx 'y '() #f)))
                (init-store))
    (and (= (length result) 2)
         (= (store-counter store) 2)
         (not (equal? (stx-ctx (car result)) (stx-ctx (cadr result)))))))

(test-end "model-core")
