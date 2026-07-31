;;; ssv/format.scm
;;; Pretty-print a source string.  Hoot has no ice-9 pretty-print, so this is a
;;; small width-directed formatter for the model's S-expression surface syntax:
;;; a form stays on one line when it fits, otherwise binding forms (lambda,
;;; let-syntax) break with a body indent and applications break one arg per line.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (ssv format)
  #:use-module (core-model)
  #:use-module (ssv source)
  #:export (format-source))

(define *width* 60)

(define (format-source src)
  (pp (string->stx src) 0))

;;; ----------------------------------------
;;; Helpers

(define (spaces n)
  (let loop ((n n) (acc ""))
    (if (<= n 0) acc (loop (- n 1) (string-append acc " ")))))

(define (string-join* lst sep)
  (if (null? lst)
      ""
      (let loop ((rest (cdr lst)) (acc (car lst)))
        (if (null? rest)
            acc
            (loop (cdr rest) (string-append acc sep (car rest)))))))

(define (atom-str x)
  (cond
   ((symbol? x) (symbol->string x))
   ((number? x) (number->string x))
   ((boolean? x) (if x "#t" "#f"))
   ((string? x) (string-append "\"" x "\""))
   (else (symbol->string x))))

;;; ----------------------------------------
;;; Rendering

(define (pp stx indent)
  (let ((form (stx-form stx)))
    (if (pair? form)
        (pp-list form indent)
        (atom-str form))))

(define (pp-list form indent)
  (let ((flat (flat-list form)))
    (if (<= (+ indent (string-length flat)) *width*)
        flat
        (pp-break form indent))))

(define (flat-list form)
  (string-append "(" (string-join* (map flat-node form) " ") ")"))

(define (flat-node stx)
  (let ((form (stx-form stx)))
    (if (pair? form) (flat-list form) (atom-str form))))

(define (pp-break form indent)
  (let ((head-sym (let ((f (stx-form (car form)))) (and (symbol? f) f)))
        (args (cdr form)))
    (cond
     ((and (eq? head-sym 'lambda) (pair? args))
      (pp-lambda (car args) (cdr args) indent))
     ((and (eq? head-sym 'let-syntax) (pair? args) (pair? (cdr args)))
      (pp-let-syntax (car args) (cadr args) (cddr args) indent))
     (else
      (pp-generic form indent)))))

;;; (lambda params body ...) — params follow the head, body indented by 2.
(define (pp-lambda params body indent)
  (let ((nl (string-append "\n" (spaces (+ indent 2)))))
    (string-append "(lambda " (pp params (+ indent 8))
                   (if (null? body)
                       ")"
                       (string-append nl
                                      (string-join*
                                       (map (lambda (b) (pp b (+ indent 2))) body)
                                       nl)
                                      ")")))))

;;; (let-syntax name transformer body ...) — name + transformer on the head line
;;; (the transformer breaks internally if needed), body indented by 2.
(define (pp-let-syntax name transformer body indent)
  (let* ((name-str (pp name 0))
         (tcol (+ indent 12 (string-length name-str) 1))
         (nl (string-append "\n" (spaces (+ indent 2)))))
    (string-append "(let-syntax " name-str " "
                   (pp transformer tcol)
                   (if (null? body)
                       ")"
                       (string-append nl
                                      (string-join*
                                       (map (lambda (b) (pp b (+ indent 2))) body)
                                       nl)
                                      ")")))))

;;; Generic application: head then one argument per line, indented by 1.
(define (pp-generic form indent)
  (let ((nl (string-append "\n" (spaces (+ indent 1)))))
    (string-append "("
                   (string-join* (map (lambda (x) (pp x (+ indent 1))) form) nl)
                   ")")))
