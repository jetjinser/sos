;;; blueprint/stencils/hoot.scm
;;; Custom blue stencil: compile Guile Scheme to WebAssembly via Guile Hoot.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (blueprint stencils hoot)
  #:use-module (blue build)
  #:use-module (blue oop)
  #:use-module (blue stencils standard configuration)
  #:use-module (blue types buildable)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:export (<hoot-wasm> hoot-wasm hoot-wasm?))

;;; Compile a Guile Scheme program into a WebAssembly module using
;;; `guild compile-wasm'.
;;;
;;; INPUTS: a single source file (the Hoot main module).
;;;
;;; OUTPUTS: a single `.wasm' file.
;;;
;;; LOAD-PATHS: list of directories passed to `guild compile-wasm -L'.
;;; Entries are typically computations such as #%~#%?srcdir and are resolved
;;; lazily by the slot getter.
;;;
;;; BUNDLE?: when true (the default), copy the Hoot web runtime (reflect.js,
;;; reflect.wasm, wtf8.wasm) next to the output via `guild compile-wasm -b'.
(define-blue-class <hoot-wasm> (<buildable>)
  (load-paths
   #:getter hoot-load-paths
   #:init-value '()
   #:init-keyword #:load-paths
   #:type list?)
  (bundle?
   #:getter hoot-bundle?
   #:init-value #t
   #:init-keyword #:bundle?
   #:type boolean?))

(define-method (ask-build-configurations (this <hoot-wasm>))
  (list %standard-configuration))

(define-method (ask-build-manifest (this <hoot-wasm>)
                                   (input <string>)
                                   (output <string>))
  (make-build-manifest
   (string-append "HOOT\t" output)
   (append (list "guild" "compile-wasm")
           (append-map (lambda (dir) (list "-L" dir))
                       (hoot-load-paths this))
           (list "-o" output)
           (if (hoot-bundle? this)
               (list "-b" (dirname output))
               '())
           (list input))))
