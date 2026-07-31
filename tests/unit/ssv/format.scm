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
  "(lambda z (let-syntax x (lambda s (MKS (LIST (syntax lambda) (syntax z) (CAR (CDR (SE s)))) (syntax here))) (x z)))")

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

(test-end "ssv-format")
