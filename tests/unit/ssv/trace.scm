;;; tests/unit/ssv/trace.scm
;;; Test the tracing infrastructure: traced runs are equivalent to direct model
;;; calls, and trace->json produces well-formed output.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (tests unit ssv trace)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 receive)
  #:use-module (core-model)
  #:use-module (phases-model)
  #:use-module (local-model)
  #:use-module (defs-model)
  #:use-module (ssv source)
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
         (member "snapshots" keys)
         (member "resolve" keys)
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

;;; ----------------------------------------
;;; annotation fields (snapshots, resolve)

(define lz-json
  (json-string->scm (trace->json (run-traced 'core (string->stx "(lambda z z)")))))

(test-assert "json-snapshots-parallel-to-steps"
  (let ((snaps (jref lz-json "snapshots"))
        (steps (jref lz-json "steps")))
    (and (vector? snaps)
         (= (vector-length snaps) (vector-length steps)))))

(test-assert "json-snapshot-entry-shape"
  (let* ((snaps (jref lz-json "snapshots"))
         (last  (vector-ref snaps (- (vector-length snaps) 1))))
    (and (> (vector-length last) 0)
         (every (lambda (e)
                  (and (vector? e)
                       (number? (vector-ref e 0))
                       (number? (vector-ref e 1))))
                (vector->list last)))))

(test-assert "json-resolve-entry-shape"
  (let ((resolve (jref lz-json "resolve")))
    (and (vector? resolve)
         (> (vector-length resolve) 0)
         (every (lambda (e)
                  (and (vector? e)
                       (number? (vector-ref e 0))
                       (number? (vector-ref e 1))
                       (string? (vector-ref e 2))))
                (vector->list resolve)))))

(test-end "ssv-trace")
