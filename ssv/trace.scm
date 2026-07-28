;;; ssv/trace.scm
;;; Drives the instrumented models to produce an expansion trace, and projects
;;; it to JSON for the frontend.  The models emit records into (ssv emit) while
;;; expanding; this module turns tracing on, runs the model, reads them back.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (ssv trace)
  #:use-module (core-model)
  #:use-module (phases-model)
  #:use-module (local-model)
  #:use-module (defs-model)
  #:use-module (ssv emit)
  #:use-module (ssv serialize)
  #:export (run-traced
            trace->json))

;;; ----------------------------------------
;;; Entry point

(define (trace-result model datum expanded store ast)
  (list (cons 'model model)
        (cons 'input datum)
        (cons 'steps (trace-records))
        (cons 'final-stx expanded)
        (cons 'final-ast ast)
        (cons 'final-store store)))

(define (run-traced model datum)
  (reset-trace!)
  (tracing-on!)
  (let ([result
         (let ([stx (as-syntax datum)])
           (case model
             [(core)
              (let* ((er (expand stx (primitives-env) (init-store)))
                     (expanded (car er)) (store (cdr er)))
                (trace-result model datum expanded store (parse expanded store)))]
             [(phases)
              (let* ([er (ph-expand 0 stx (primitives-env) '() (init-store))]
                     [expanded (car er)]
                     [store (cdr er)])
                (trace-result model datum expanded store (ph-parse 0 expanded store)))]
             [(local)
              (let* ((er (loc-expand 0 stx (primitives-env) (list (init-store) '() '())))
                     (expanded (car er)) (store (cadr er)))
                (trace-result model datum expanded store (ph-parse 0 expanded store)))]
             [(defs)
              (let* ((er (defs-expand 0 stx (primitives-env) (list (init-store) '() '())))
                     (expanded (car er)) (store (cadr er)))
                (trace-result model datum expanded store (ph-parse 0 expanded store)))]
             [else (error "run-traced: unknown model" model)]))])
    (tracing-off!)
    result))

;;; ----------------------------------------
;;; JSON projection (named fields, for the frontend)

(define (info->json info)
  (json-object
   (map (lambda (e) (cons (symbol->string (car e)) (value->json (cdr e))))
        info)))

(define (record->json rec)
  (case (car rec)
    [(rule)
     (json-object
      (list (cons "type"   (json-string "rule"))
            (cons "rule"   (json-string (symbol->string (cadr rec))))
            (cons "before" (value->json (caddr rec)))
            (cons "after"  (value->json (cadddr rec)))
            (cons "store"  (value->json (list-ref rec 4)))
            (cons "env"    (value->json (list-ref rec 5)))
            (cons "info"   (info->json (list-ref rec 6)))))]
    [(op)
     (json-object
      (list (cons "type" (json-string "op"))
            (cons "op"   (json-string (symbol->string (cadr rec))))
            (cons "data" (value->json (cddr rec)))))]
    [else (value->json rec)]))

(define (trace->json tr)
  (json-object
    (list (cons "model"       (json-string (symbol->string (cdr (assq 'model tr)))))
          (cons "input"       (value->json (cdr (assq 'input tr))))
          (cons "steps"       (json-array (map record->json (cdr (assq 'steps tr)))))
          (cons "final-stx"   (value->json (cdr (assq 'final-stx tr))))
          (cons "final-ast"   (value->json (cdr (assq 'final-ast tr))))
          (cons "final-store" (value->json (cdr (assq 'final-store tr)))))))
