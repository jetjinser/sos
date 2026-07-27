;;; ssv/emit.scm
;;; Instrumentation substrate: a trace accumulator the models emit into while
;;; expanding.  Recording is gated by *tracing* (off by default), so the models
;;; run clean unless a tracer is active.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (ssv emit)
  #:export (*tracing*
            tracing-on! tracing-off!
            reset-trace! trace-records
            emit! emit-rule emit-op))

(define *tracing* #f)
(define *trace* '())

(define (tracing-on!)  (set! *tracing* #t))
(define (tracing-off!) (set! *tracing* #f))

(define (reset-trace!)  (set! *trace* '()))
(define (trace-records) (reverse *trace*))

(define (emit! rec)
  (if *tracing* (set! *trace* (cons rec *trace*))))

;;; A rule application: RULE fired, transforming BEFORE into AFTER under STORE
;;; and ENV, with rule-specific INFO.
(define (emit-rule rule before after store env info)
  (if *tracing*
      (set! *trace* (cons (list 'rule rule before after store env info)
                          *trace*))))

;;; A primitive operation (scope add/flip, bind, allocation, ...).
(define (emit-op kind . data)
  (if *tracing*
      (set! *trace* (cons (cons 'op (cons kind data)) *trace*))))
