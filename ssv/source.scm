;;; ssv/source.scm
;;; Position-aware S-expression reader: parses a source string into an stx tree
;;; whose nodes carry (start . end) character-offset spans.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (ssv source)
  #:use-module (core-model)
  #:use-module (ice-9 receive)
  #:export (string->stx))

(define (ws? c)
  (or (char=? c #\space) (char=? c #\tab)
      (char=? c #\newline) (char=? c #\return)))

(define (delim? c)
  (or (ws? c) (char=? c #\() (char=? c #\))))

(define (skip src pos len)
  (if (>= pos len)
      pos
      (let ((c (string-ref src pos)))
        (cond
         ((ws? c) (skip src (+ pos 1) len))
         ((char=? c #\;)
          (let loop ((p pos))
            (if (or (>= p len) (char=? (string-ref src p) #\newline))
                (skip src p len)
                (loop (+ p 1)))))
         (else pos)))))

(define (atom-end src pos len)
  (if (or (>= pos len) (delim? (string-ref src pos)))
      pos
      (atom-end src (+ pos 1) len)))

(define (classify text)
  (cond
   ((string=? text "#t") #t)
   ((string=? text "#f") #f)
   ((string->number text))
   (else (string->symbol text))))

(define (read-form src pos len)
  (let ((pos (skip src pos len)))
    (if (>= pos len)
        (error "string->stx: unexpected end of input")
        (let ((c (string-ref src pos)))
          (cond
           ((char=? c #\() (read-list src (+ pos 1) len '() pos))
           ((char=? c #\') (read-quote src (+ pos 1) len pos))
           (else           (read-atom src pos len)))))))

(define (read-list src pos len elems start)
  (let ((pos (skip src pos len)))
    (cond
     ((>= pos len)
      (error "string->stx: unclosed parenthesis"))
     ((char=? (string-ref src pos) #\))
      (values (make-stx (reverse elems) '() (cons start (+ pos 1)))
              (+ pos 1)))
     (else
      (receive (elem pos2) (read-form src pos len)
        (read-list src pos2 len (cons elem elems) start))))))

(define (read-quote src pos len start)
  (receive (inner pos2) (read-form src pos len)
    (values (make-stx (list (make-stx 'quote '() (cons start (+ start 1)))
                            inner)
                      '()
                      (cons start pos2))
            pos2)))

(define (read-atom src pos len)
  (let ((end (atom-end src pos len)))
    (values (make-stx (classify (substring src pos end))
                      '()
                      (cons pos end))
            end)))

(define (string->stx src)
  (receive (stx _) (read-form src 0 (string-length src))
    stx))
