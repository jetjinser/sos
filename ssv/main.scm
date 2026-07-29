;;; ssv/main.scm
;;; Hoot main module.  JavaScript calls these procedures through the reflect
;;; bridge (Scheme.eval); each returns a JSON string.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(use-modules (ssv source) (ssv trace))

(define (run-model model-name input-src)
  (trace->json (run-traced (string->symbol model-name)
                           (string->stx input-src))))

;;; Hand the entry point to the host (Scheme.load_main resolves to it).
run-model
