;;; ssv/main.scm
;;; Hoot main module.  JavaScript calls these procedures through the reflect
;;; bridge; run-model returns the trace JSON, format-src pretty-prints source.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(use-modules (ssv source) (ssv trace) (ssv format) (ssv syntax-rules))

(define (run-model model-name input-src)
  (trace->json (run-traced (string->symbol model-name)
                           (string->stx input-src))))

(define (format-src input-src)
  (format-source input-src))

;;; Hand the entry points to the host (Scheme.load_main resolves to them).
(values run-model format-src)
