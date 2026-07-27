;;; ssv/main.scm
;;; Hoot main module.  JavaScript calls these procedures through the reflect
;;; bridge (Scheme.eval); each returns a JSON string.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(use-modules (ssv trace))

(define (read-from-string s)
  (let ((p (open-input-string s)))
    (let ((d (read p)))
      (close-port p)
      d)))

;;; Expand the datum read from INPUT-SRC under MODEL-NAME; return the trace JSON.
(define (run-model model-name input-src)
  (trace->json (run-traced (string->symbol model-name)
                           (read-from-string input-src))))

;;; Hand the entry point to the host (Scheme.load_main resolves to it).
run-model
