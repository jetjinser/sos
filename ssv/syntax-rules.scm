;;; ssv/syntax-rules.scm
;;; syntax-rules as a library macro built on the expander's syntax API.
;;; Given a (syntax-rules (lit ...) (pattern template)) form, it compiles a
;;; matcher transformer (lambda use ...) whose body deconstructs the use-site
;;; with syntax->datum/CAR/CDR and rebuilds the template with datum->syntax,
;;; so the hygiene primitives stay visible in the expansion trace.
;;; Scope: a single clause; pattern variables and `_`; one trailing `var ...`
;;; ellipsis that matches a suffix and splices it back into the template.
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

;;; (drop-expr e n): expression for e with its first n elements removed.
(define (drop-expr e n)
  (let loop ((f `(syntax->datum ,e)) (k n))
    (if (= k 0) f (loop `(CDR ,f) (- k 1)))))

;;; Collect pattern variables with their index paths, skipping `_`, the
;;; ellipsis marker `...`, and the ellipsis variable SKIP.
(define (collect-vars p rpath acc skip)
  (cond
    ((null? p) acc)
    ((pair? p)
     (let walk ((lst p) (idx 0) (acc acc))
       (cond
         ((null? lst) acc)
         ((pair? lst)
          (let ((e (car lst)))
            (cond
              ((eq? e '...) (walk (cdr lst) (+ idx 1) acc))
              ((and (symbol? e) (eq? e skip)) (walk (cdr lst) (+ idx 1) acc))
              (else (walk (cdr lst) (+ idx 1)
                          (collect-vars e (cons idx rpath) acc skip))))))
         (else acc))))
    ((eq? p '_) acc)
    ((eq? p '...) acc)
    ((symbol? p) (cons (cons p (reverse rpath)) acc))
    (else acc)))

;;; If the top-level pattern ends with (var ...), return (var . start-index).
(define (trailing-ellipsis pat)
  (let loop ((lst (cdr pat)) (idx 1))
    (cond
      ((or (null? lst) (null? (cdr lst))) #f)
      ((and (symbol? (car lst)) (eq? (cadr lst) '...)) (cons (car lst) idx))
      ((pair? (car lst)) #f)
      (else (loop (cdr lst) (+ idx 1))))))

;;; If the template list ends with (var ...), return (prefix . var).
(define (trailing-splice tmpl)
  (if (and (pair? tmpl) (pair? (cdr tmpl)) (eq? (car (reverse tmpl)) '...))
      (let ((rev (reverse tmpl)))
        (if (symbol? (cadr rev))
            (cons (reverse (cddr rev)) (cadr rev))
            #f))
      #f))

;;; CONS chain splicing SEQ after the rendered PREFIX elements.
(define (splice-expr prefix seq vars evar)
  (if (null? prefix)
      seq
      `(CONS ,(template-expr (car prefix) vars evar seq)
             ,(splice-expr (cdr prefix) seq vars evar))))

;;; Build the template expression.  VARS maps pattern variables to paths; EVAR
;;; (with sequence expression SEQ) is the ellipsis variable.  Every compound
;;; node is wrapped in datum->syntax; a list ending in (EVAR ...) splices SEQ.
(define (template-expr tmpl vars evar seq)
  (cond
    ((null? tmpl) '(LIST))
    ((pair? tmpl)
     (let ((sp (trailing-splice tmpl)))
       (if (and sp (eq? (cdr sp) evar))
           `(datum->syntax use ,(splice-expr (car sp) seq vars evar))
           `(datum->syntax use
                           (LIST ,@(map (lambda (e) (template-expr e vars evar seq))
                                        tmpl))))))
    ((assq tmpl vars) => (lambda (e) (path-expr 'use (cdr e))))
    (else `(syntax ,tmpl))))

;;; Compile a single (pattern template) clause into a matcher datum.
(define (compile-clause clause)
  (let* ((pat (car clause))
         (tmpl (cadr clause))
         (einfo (trailing-ellipsis pat))
         (evar (and einfo (car einfo)))
         (seq (and einfo (drop-expr 'use (cdr einfo))))
         (vars (collect-vars pat '() '() evar)))
    `(lambda use ,(template-expr tmpl vars evar seq))))

;;; The syntax-rules transformer: reads the clause from the use-site and
;;; returns the compiled matcher as syntax.
(define (syntax-rules-transformer use-stx)
  (let* ((form (strip use-stx))
         (matcher (compile-clause (caddr form))))
    (string->stx (call-with-output-string
                  (lambda (p) (write matcher p))))))

(register-for-syntax! 'syntax-rules syntax-rules-transformer)
