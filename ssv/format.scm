;;; ssv/format.scm
;;; Pretty-print a source string.  Hoot has no ice-9 pretty-print, so this is a
;;; small width-directed formatter for the model's S-expression surface syntax.
;;; Breaking is deliberately aggressive for readability: binding forms (lambda,
;;; let-syntax) break as soon as a body or transformer subform is compound,
;;; (syntax ...) breaks around any compound expression, and other applications
;;; break one arg per line unless short.  (syntax-rules ...) and
;;; (syntax-case ...) keep their literals (and subject) with the head and
;;; break each clause to its own line.  Quoted data never breaks.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (ssv format)
  #:use-module (core-model)
  #:use-module (ssv source)
  #:export (format-source))

;;; Absolute line budget, and the size under which a nested application may
;;; stay inline even though it contains subforms.
(define *width* 60)
(define *compact* 36)

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
   ((null? x) "()")
   ((symbol? x) (symbol->string x))
   ((number? x) (number->string x))
   ((boolean? x) (if x "#t" "#f"))
   ((string? x) (string-append "\"" x "\""))
   (else (symbol->string x))))

(define (atom-stx? stx) (not (pair? (stx-form stx))))

(define (all-atoms? lst)
  (or (null? lst)
      (and (atom-stx? (car lst)) (all-atoms? (cdr lst)))))

(define (fits? flat indent)
  (<= (+ indent (string-length flat)) *width*))

(define (head-sym form)
  (let ((f (stx-form (car form)))) (and (symbol? f) f)))

;;; ----------------------------------------
;;; Rendering

(define (pp stx indent)
  (let ((form (stx-form stx)))
    (if (pair? form)
        (pp-list form indent)
        (atom-str form))))

(define (pp-list form indent)
  (let ((flat (flat-list form))
        (head (head-sym form))
        (args (cdr form)))
    (cond
     ((eq? head 'quote) flat)
     ((and (eq? head 'lambda) (pair? args))
      (if (and (fits? flat indent) (all-atoms? (cdr args)))
          flat
          (pp-lambda (car args) (cdr args) indent)))
     ((and (eq? head 'let-syntax) (pair? args) (pair? (cdr args)))
      (if (and (fits? flat indent)
               (atom-stx? (cadr args))
               (all-atoms? (cddr args)))
          flat
          (pp-let-syntax (car args) (cadr args) (cddr args) indent)))
     ((and (eq? head 'syntax) (pair? args))
      (if (and (fits? flat indent) (all-atoms? args))
          flat
          (pp-generic form indent)))
     ((and (eq? head 'syntax-rules) (pair? args))
      (if (and (fits? flat indent) (<= (string-length flat) *compact*))
          flat
          (pp-syntax-rules (car args) (cdr args) indent)))
     ((and (eq? head 'syntax-case) (pair? args) (pair? (cdr args)))
      (if (and (fits? flat indent) (<= (string-length flat) *compact*))
          flat
          (pp-syntax-case (car args) (cadr args) (cddr args) indent)))
     ((and (fits? flat indent)
           (or (<= (string-length flat) *compact*) (all-atoms? args)))
      flat)
     (else (pp-generic form indent)))))

(define (flat-list form)
  (string-append "(" (string-join* (map flat-node form) " ") ")"))

(define (flat-node stx)
  (let ((form (stx-form stx)))
    (if (pair? form) (flat-list form) (atom-str form))))

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

;;; (let-syntax name transformer body ...) — transformer and body each break
;;; to their own line, indented by 2.
(define (pp-let-syntax name transformer body indent)
  (let ((nl (string-append "\n" (spaces (+ indent 2)))))
    (string-append "(let-syntax " (pp name 0)
                   nl (pp transformer (+ indent 2))
                   (if (null? body)
                       ")"
                       (string-append nl
                                      (string-join*
                                       (map (lambda (b) (pp b (+ indent 2))) body)
                                       nl)
                                      ")")))))

;;; (syntax-rules literals clause ...) — literals stay with the head, each
;;; clause breaks to its own line, indented by 2.
(define (pp-syntax-rules literals clauses indent)
  (let ((nl (string-append "\n" (spaces (+ indent 2)))))
    (string-append "(syntax-rules " (pp literals (+ indent 14))
                   (if (null? clauses)
                       ")"
                       (string-append nl
                                      (string-join*
                                       (map (lambda (c) (pp-clause c (+ indent 2))) clauses)
                                       nl)
                                      ")")))))

;;; (syntax-case subject literals clause ...) — subject and literals stay
;;; with the head, each clause breaks to its own line, indented by 2.
(define (pp-syntax-case subject literals clauses indent)
  (let ((nl (string-append "\n" (spaces (+ indent 2)))))
    (string-append "(syntax-case " (pp subject indent)
                   " " (pp literals indent)
                   (if (null? clauses)
                       ")"
                       (string-append nl
                                      (string-join*
                                       (map (lambda (c) (pp-clause c (+ indent 2))) clauses)
                                       nl)
                                      ")")))))

;;; A clause stays on one line when it fits the width budget.
(define (pp-clause stx indent)
  (let ((flat (flat-node stx)))
    (if (fits? flat indent) flat (pp stx indent))))

;;; Generic application: head then one argument per line, indented by 1.
(define (pp-generic form indent)
  (let ((nl (string-append "\n" (spaces (+ indent 1)))))
    (string-append "("
                   (string-join* (map (lambda (x) (pp x (+ indent 1))) form) nl)
                   ")")))
