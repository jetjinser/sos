;;; ssv/syntax-rules.scm
;;; syntax-rules as a library macro built on the expander's syntax API.
;;; Given a (syntax-rules (lit ...) (pattern template)) form, it compiles a
;;; matcher transformer (lambda use ...) whose body deconstructs the use-site
;;; with syntax->datum/CAR/CDR and rebuilds the template with datum->syntax,
;;; so the hygiene primitives stay visible in the expansion trace.
;;; Step 5b scope: a single clause, pattern variables, `_`; no ellipsis yet.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (ssv syntax-rules)
  #:use-module (core-model)
  #:use-module (ssv source)
  #:export (syntax-rules-transformer))

;;; Strip a stx tree to a plain datum.
(define (strip s)
  (let ((f (stx-form s)))
    (if (pair? f) (map strip f) f)))

;;; (nth-expr e i): expression selecting the i-th element of the stx e.  The
;;; form produced by syntax->datum is itself stx-nested, so every descent must
;;; re-apply syntax->datum to the sub-stx.
(define (nth-expr e i)
  (let loop ((f `(syntax->datum ,e)) (k i))
    (if (= k 0) `(CAR ,f) (loop `(CDR ,f) (- k 1)))))

;;; (path-expr base path): expression reaching use[path] from base.
(define (path-expr base path)
  (if (null? path)
      base
      (path-expr (nth-expr base (car path)) (cdr path))))

;;; Collect pattern variables with their index paths.  `_` and non-symbols are
;;; ignored; index 0 of the pattern is the macro keyword.
(define (collect-vars p rpath acc)
  (cond
    ((null? p) acc)
    ((pair? p)
     (let walk ((lst p) (idx 0) (acc acc))
       (cond
         ((null? lst) acc)
         ((pair? lst)
          (walk (cdr lst) (+ idx 1)
                (collect-vars (car lst) (cons idx rpath) acc)))
         (else acc))))
    ((eq? p '_) acc)
    ((symbol? p) (cons (cons p (reverse rpath)) acc))
    (else acc)))

;;; Build the template expression.  Every compound node is wrapped in
;;; datum->syntax so the result is a proper stx tree; pattern variables become
;;; access expressions, other symbols become (syntax ...) literals.
(define (template-expr tmpl vars)
  (cond
    ((null? tmpl) '(LIST))
    ((pair? tmpl)
     `(datum->syntax use (LIST ,@(map (lambda (e) (template-expr e vars)) tmpl))))
    ((assq tmpl vars) => (lambda (e) (path-expr 'use (cdr e))))
    (else `(syntax ,tmpl))))

;;; Compile a single (pattern template) clause into a matcher datum.
(define (compile-clause clause)
  (let ((vars (collect-vars (car clause) '() '())))
    `(lambda use ,(template-expr (cadr clause) vars))))

;;; The syntax-rules transformer: reads the clause from the use-site and
;;; returns the compiled matcher as syntax.
(define (syntax-rules-transformer use-stx)
  (let* ((form (strip use-stx))
         (matcher (compile-clause (caddr form))))
    (string->stx (call-with-output-string
                  (lambda (p) (write matcher p))))))

(register-for-syntax! 'syntax-rules syntax-rules-transformer)
