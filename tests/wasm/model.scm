;;; tests/wasm/model.scm
;;; Run the scope-set model test suites inside the Hoot WebAssembly VM.
;;;
;;; This is a Hoot main module (program syntax).  It is compiled to Wasm and
;;; executed headlessly with:
;;;   guild compile-wasm -L . -L model --run tests/wasm/model.scm
;;;
;;; The default Hoot VM prints the program's return values (not `display'
;;; output), so on success we RETURN a summary, and on failure we raise an
;;; error, which makes the VM exit with a non-zero status.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(use-modules (core-model)
             (phases-model)
             (local-model)
             (defs-model)
             (tests unit model harness))

(define (core-run in)
  (let ((er (expand (as-syntax in) (primitives-env) (init-store))))
    (parse (car er) (cdr er))))

(define (phases-run in)
  (let ((er (ph-expand 0 (as-syntax in) (primitives-env) '() (init-store))))
    (ph-parse 0 (car er) (cdr er))))

;;; Σ* = (store scps-p scps-u); the store is the cadr of the result pair.
(define (local-run in)
  (let ((er (loc-expand 0 (as-syntax in) (primitives-env)
                        (list (init-store) '() '()))))
    (ph-parse 0 (car er) (cadr er))))

(define (defs-run in)
  (let ((er (defs-expand 0 (as-syntax in) (primitives-env)
                         (list (init-store) '() '()))))
    (ph-parse 0 (car er) (cadr er))))

(define failed '())

(define (check name run input expected)
  (if (term=? (run input) expected)
      #t
      (set! failed (cons name failed))))

;;; core (5)
(check "core/simple-macro"   core-run input-simple-macro 2)
(check "core/reftrans-macro" core-run input-reftrans
       '(fun (var z:0) (fun (var z:4) (var z:0))))
(check "core/hyg-macro"      core-run input-hyg
       '(fun (var z:0) (fun (var z:6) (var z:0))))
(check "core/thunk"          core-run input-thunk
       '(app (app (fun (var a:2) (fun (var a:6) (app + (var a:2) 1))) 5) 0))
(check "core/get-identity"   core-run input-get-identity
       '(fun (var a:4) (fun (var a:6) (var a:4))))

;;; phases (7)
(check "phases/simple-macro"   phases-run input-simple-macro 2)
(check "phases/reftrans-macro" phases-run input-reftrans
       '(fun (var z:0) (fun (var z:6) (var z:0))))
(check "phases/hyg-macro"      phases-run input-hyg
       '(fun (var z:0) (fun (var z:8) (var z:0))))
(check "phases/thunk"          phases-run input-thunk
       '(app (app (fun (var a:4) (fun (var a:8) (app + (var a:4) 1))) 5) 0))
(check "phases/get-identity"   phases-run input-get-identity
       '(fun (var a:6) (fun (var a:8) (var a:6))))
(check "phases/prune"          phases-run input-prune 3)
(check "phases/gen"            phases-run input-gen '(fun (var y:10) (var y:10)))

;;; local (11)
(check "local/simple-macro"      local-run input-simple-macro 2)
(check "local/reftrans-macro"    local-run input-reftrans
       '(fun (var z:0) (fun (var z:6) (var z:0))))
(check "local/hyg-macro"         local-run input-hyg
       '(fun (var z:0) (fun (var z:8) (var z:0))))
(check "local/thunk"             local-run input-thunk
       '(app (app (fun (var a:4) (fun (var a:8) (app + (var a:4) 1))) 5) 0))
(check "local/get-identity"      local-run input-get-identity
       '(fun (var a:6) (fun (var a:8) (var a:6))))
(check "local/prune"             local-run input-prune 3)
(check "local/gen"               local-run input-gen '(fun (var y:10) (var y:10)))
(check "local/local-value"       local-run input-local-value 8)
(check "local/local-expand"      local-run input-local-expand 8)
(check "local/local-expand-stop" local-run input-local-expand-stop 8)
(check "local/local-binder"      local-run input-local-binder
       '(fun (var x:12) (var x:12)))

;;; defs (15)
(check "defs/simple-macro"      defs-run input-simple-macro 2)
(check "defs/reftrans-macro"    defs-run input-reftrans
       '(fun (var z:0) (fun (var z:6) (var z:0))))
(check "defs/hyg-macro"         defs-run input-hyg
       '(fun (var z:0) (fun (var z:8) (var z:0))))
(check "defs/thunk"             defs-run input-thunk
       '(app (app (fun (var a:4) (fun (var a:8) (app + (var a:4) 1))) 5) 0))
(check "defs/get-identity"      defs-run input-get-identity
       '(fun (var a:6) (fun (var a:8) (var a:6))))
(check "defs/prune"             defs-run input-prune 3)
(check "defs/gen"               defs-run input-gen '(fun (var y:10) (var y:10)))
(check "defs/local-value"       defs-run input-local-value 8)
(check "defs/local-expand"      defs-run input-local-expand 8)
(check "defs/local-expand-stop" defs-run input-local-expand-stop 8)
(check "defs/local-binder"      defs-run input-local-binder
       '(fun (var x:12) (var x:12)))
(check "defs/box"               defs-run input-box 0)
(check "defs/set-box"           defs-run input-set-box 1)
(check "defs/defs-shadow"       defs-run input-defs-shadow
       '(fun (var p:23) (app (var p:23))))
(check "defs/defs-local-macro"  defs-run input-defs-local-macro
       '(fun (var p:27) 13))

(if (pair? failed)
    (error "model Wasm tests failed" (reverse failed))
    '(all-model-tests-pass (core . 5) (phases . 7) (local . 11) (defs . 15)))
