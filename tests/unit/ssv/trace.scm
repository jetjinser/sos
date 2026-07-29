;;; tests/unit/ssv/trace.scm
;;; Test the tracing infrastructure: traced runs are equivalent to direct model
;;; calls, and trace->json produces well-formed output.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (tests unit ssv trace)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 receive)
  #:use-module (core-model)
  #:use-module (phases-model)
  #:use-module (local-model)
  #:use-module (defs-model)
  #:use-module (ssv trace)
  #:use-module (tests unit model harness)
  #:use-module (json))

(test-begin "ssv-trace")

;;; ----------------------------------------
;;; Equivalence: tracing is a pure side channel

(test-assert "core-equivalence"
  (receive (expanded store) (expand input-hyg (primitives-env) (init-store))
    (let ((tr (run-traced 'core input-hyg)))
      (and (term=? (cdr (assq 'final-stx tr)) expanded)
           (term=? (cdr (assq 'final-ast tr)) (parse expanded store))))))

(test-assert "phases-equivalence"
  (receive (expanded store) (ph-expand 0 input-hyg (primitives-env) '() (init-store))
    (let ((tr (run-traced 'phases input-hyg)))
      (and (term=? (cdr (assq 'final-stx tr)) expanded)
           (term=? (cdr (assq 'final-ast tr)) (ph-parse 0 expanded store))))))

(test-assert "local-equivalence"
  (receive (expanded s*) (loc-expand 0 input-local-expand (primitives-env)
                                     (list (init-store) '() '()))
    (let ((tr (run-traced 'local input-local-expand)))
      (and (term=? (cdr (assq 'final-stx tr)) expanded)
           (term=? (cdr (assq 'final-ast tr))
                   (ph-parse 0 expanded (car s*)))))))

(test-assert "defs-equivalence"
  (receive (expanded s*) (defs-expand 0 input-defs-shadow (primitives-env)
                                      (list (init-store) '() '()))
    (let ((tr (run-traced 'defs input-defs-shadow)))
      (and (term=? (cdr (assq 'final-stx tr)) expanded)
           (term=? (cdr (assq 'final-ast tr))
                   (ph-parse 0 expanded (car s*)))))))

;;; ----------------------------------------
;;; trace->json structure

(define (jref obj key)
  (let ((e (assoc key obj)))
    (and e (cdr e))))

(test-assert "json-top-level-keys"
  (let* ((json (json-string->scm (trace->json (run-traced 'core input-simple-macro))))
         (keys (map car json)))
    (and (member "model" keys)
         (member "input" keys)
         (member "steps" keys)
         (member "final-stx" keys)
         (member "final-ast" keys)
         (member "final-store" keys))))

(test-assert "json-model-name"
  (let ((json (json-string->scm (trace->json (run-traced 'core input-simple-macro)))))
    (equal? (jref json "model") "core")))

(test-assert "json-steps-nonempty"
  (let ((steps (jref (json-string->scm (trace->json (run-traced 'core input-simple-macro)))
                     "steps")))
    (and (vector? steps)
         (> (vector-length steps) 0))))

(test-assert "json-step-types"
  (let* ((steps (jref (json-string->scm (trace->json (run-traced 'core input-simple-macro)))
                      "steps"))
         (types (map (lambda (s) (jref s "type"))
                     (vector->list steps))))
    (and (member "rule" types)
         (member "op" types))))

(test-end "ssv-trace")
