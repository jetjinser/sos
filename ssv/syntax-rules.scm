;;; ssv/syntax-rules.scm
;;; syntax-rules as a library macro built on the expander's syntax API.
;;; Given a (syntax-rules (lit ...) (pattern template) ...) form, it desugars
;;; to the primitive matcher (lambda use (syntax-case use (lit ...) ...)),
;;; wrapping each template in (syntax ...): pattern matching, literal checks
;;; and ellipsis splicing all happen in the syntax-case/template primitives,
;;; so the hygiene machinery stays visible in the expansion trace.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (ssv syntax-rules)
  #:use-module (core-model)
  #:use-module (ssv source)
  #:export (syntax-rules-transformer))

;;; Strip a stx tree to a plain datum.
(define (strip s)
  (let ((f (stx-form s)))
    (if (pair? f) (map strip f) f)))

;;; Rebuild a stx tree with every span cleared.  The matcher is fresh syntax
;;; (not part of the user's source), so its string->stx parse positions must
;;; not leak into the source-span snapshots.
(define (no-spans s)
  (let ((f (stx-form s)))
    (make-stx (if (pair? f) (map no-spans f) f)
              (stx-ctx s)
              #f)))

;;; The syntax-rules transformer: reads the literals and clauses from the
;;; use-site and returns the desugared syntax-case matcher as syntax.
(define (syntax-rules-transformer use-stx)
  (let* ((form (strip use-stx))
         (matcher `(lambda use
                     (syntax-case use ,(cadr form)
                       ,@(map (lambda (clause)
                                (list (car clause)
                                      `(syntax ,(cadr clause))))
                              (cddr form))))))
    (no-spans (string->stx (call-with-output-string
                            (lambda (p) (write matcher p)))))))

(register-for-syntax! 'syntax-rules syntax-rules-transformer)
