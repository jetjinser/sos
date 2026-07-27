;;; blueprint/serve.scm
;;; A blue command serving the built web app with the Hoot development server.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (blueprint serve)
  #:use-module (blue computation)
  #:use-module (blue types command)
  #:export (serve-command))

(define-command (serve-command _)
  ((invoke "serve")
   (category 'dev)
   (synopsis "Serve the built web app")
   (help "Run the Hoot development web server on the built web application (builddir/web)."))
  (let* ((builddir (run-computation #%~#%?builddir))
         (web-dir  (string-append builddir "/web"))
         (serve    (@ (hoot web-server) serve)))
    (serve #:work-dir web-dir)))
