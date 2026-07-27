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

;;; Compile a Guile Scheme program to a WebAssembly module via
;;; `guild compile-wasm', optionally bundling the Hoot web runtime alongside.
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
