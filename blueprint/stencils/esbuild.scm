;;; blueprint/stencils/esbuild.scm
;;; Custom blue stencil: bundle JavaScript modules via esbuild.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (blueprint stencils esbuild)
  #:use-module (blue build)
  #:use-module (blue oop)
  #:use-module (blue stencils standard configuration)
  #:use-module (blue types buildable)
  #:use-module (oop goops)
  #:export (<esbuild-bundle> esbuild-bundle))

(define-blue-class <esbuild-bundle> (<buildable>)
  (format
   #:getter esbuild-format
   #:init-value "esm"
   #:init-keyword #:format
   #:type string?)
  (extra-args
   #:getter esbuild-extra-args
   #:init-value '()
   #:init-keyword #:extra-args
   #:type list?))

(define-method (ask-build-configurations (this <esbuild-bundle>))
  (list %standard-configuration))

(define-method (ask-build-manifest (this <esbuild-bundle>)
                                   (input <string>)
                                   (output <string>))
  (make-build-manifest
   (string-append "ESBUILD\t" output)
   (append (list "esbuild" input "--bundle"
                 (string-append "--format=" (esbuild-format this))
                 (string-append "--outfile=" output))
           (esbuild-extra-args this))))
