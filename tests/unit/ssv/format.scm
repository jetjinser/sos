;;; tests/unit/ssv/format.scm
;;; Test the pretty-printer: it preserves the parsed structure, is idempotent,
;;; and breaks long forms across lines.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (tests unit ssv format)
  #:use-module (srfi srfi-64)
  #:use-module (core-model)
  #:use-module (ssv source)
  #:use-module (ssv format))

(test-begin "ssv-format")

;;; Parsed structure with spans stripped, for semantic comparison.
(define (plain stx)
  (let ((f (stx-form stx))) (if (pair? f) (map plain f) f)))

(define (datum s) (plain (string->stx s)))

(define long-input
  "(lambda z (let-syntax x (lambda s (datum->syntax (syntax here) (LIST (syntax lambda) (syntax z) (CAR (CDR (syntax->datum s)))))) (x z)))")

(test-assert "preserves-semantics"
  (equal? (datum long-input) (datum (format-source long-input))))

(test-assert "idempotent"
  (let ((f (format-source long-input)))
    (equal? f (format-source f))))

(test-equal "short-stays-inline"
  "(x 1)"
  (format-source "(x 1)"))

(test-assert "long-form-breaks"
  (let ((f (format-source long-input)))
    (and (member #\newline (string->list f))
         (equal? (datum f) (datum long-input)))))

;;; ----------------------------------------
;;; Aggressive breaking

(test-equal "lambda-breaks-on-compound-body"
  "(lambda (x)\n  (CONS x x))"
  (format-source "(lambda (x) (CONS x x))"))

(test-equal "lambda-atomic-body-stays-inline"
  "(lambda (x) x)"
  (format-source "(lambda (x) x)"))

(test-equal "let-syntax-transformer-own-line"
  "(let-syntax x\n  (lambda z\n    (syntax\n     (quote 2)))\n  (x 1))"
  (format-source "(let-syntax x (lambda z (syntax (quote 2))) (x 1))"))

(test-equal "syntax-breaks-around-compound"
  "(syntax\n (lambda (x) x))"
  (format-source "(syntax (lambda (x) x))"))

(test-equal "compact-app-stays-inline"
  "(CAR (CDR (syntax->datum s)))"
  (format-source "(CAR (CDR (syntax->datum s)))"))

(test-equal "short-syntax-rules-stays-inline"
  "(syntax-rules (if) ((_ x) x))"
  (format-source "(syntax-rules (if) ((_ x) x))"))

(test-equal "syntax-rules-literals-with-head"
  "(syntax-rules ()\n  ((_ (x v) body) ((lambda x body) v)))"
  (format-source "(syntax-rules () ((_ (x v) body) ((lambda x body) v)))"))

(test-equal "syntax-rules-clauses-break"
  "(syntax-rules ()\n  ((_ a b) (CONS a b))\n  ((_ a b c) (LIST a b c)))"
  (format-source "(syntax-rules () ((_ a b) (CONS a b)) ((_ a b c) (LIST a b c)))"))

(test-equal "syntax-case-clauses-break"
  "(syntax-case use ()\n  ((_ (x v) body) (syntax ((lambda x body) v))))"
  (format-source "(syntax-case use () ((_ (x v) body) (syntax ((lambda x body) v))))"))

(test-assert "quote-never-breaks"
  (let ((f (format-source
            "(quote (aaaaaaaaaa bbbbbbbbbb cccccccccc dddddddddd eeeeeeeeee))")))
    (not (member #\newline (string->list f)))))

(test-end "ssv-format")
