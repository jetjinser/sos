;;; ssv/serialize.scm
;;; JSON serialization of scope-set model data structures.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (ssv serialize)
  #:use-module (core-model)
  #:export (stx->json
            store->json
            ctx->json
            env->json
            value->json
            json-string
            json-escape-string
            json-array
            json-object
            json-join))

;;; ----------------------------------------
;;; JSON primitives

(define (json-escape-string s)
  (apply string-append
         (map (lambda (c)
                (cond
                 ((char=? c #\") "\\\"")
                 ((char=? c #\\) "\\\\")
                 ((char=? c #\newline) "\\n")
                 ((char=? c #\return) "\\r")
                 ((char=? c #\tab) "\\t")
                 (else (string c))))
              (string->list s))))

(define (json-string s)
  (string-append "\"" (json-escape-string s) "\""))

(define (json-symbol sym)
  (json-string (symbol->string sym)))

(define (json-join strs)
  (let loop ((ss strs) (acc ""))
    (cond
     ((null? ss) acc)
     ((null? (cdr ss)) (string-append acc (car ss)))
     (else (loop (cdr ss) (string-append acc (car ss) ","))))))

(define (json-array elems)
  (string-append "[" (json-join elems) "]"))

(define (json-object entries)
  (string-append "{"
                 (json-join
                  (map (lambda (e)
                         (string-append (json-string (car e)) ":" (cdr e)))
                       entries))
                 "}"))

(define (json-number n)
  (number->string n))

;;; ----------------------------------------
;;; Scope sets and contexts

(define (scopes->json scps)
  (json-array (map json-symbol scps)))

;;; A ctx is either a plain scope set (core) or a phase-indexed map (phases+);
;;; the shape is detected from the first entry.
(define (ctx->json ctx)
  (cond
   ((null? ctx) "[]")
   ((and (pair? ctx) (pair? (car ctx)) (integer? (caar ctx)))
    (json-array
     (map (lambda (entry)
            (json-array (list (json-number (car entry))
                              (scopes->json (cdr entry)))))
          ctx)))
   (else (scopes->json ctx))))

;;; ----------------------------------------
;;; Syntax objects

(define (stx->json stx)
  (json-object
   (list (cons "form" (value->json (stx-form stx)))
         (cons "ctx"  (ctx->json (stx-ctx stx))))))

;;; ----------------------------------------
;;; Store

(define (binds->json binds)
  (json-object
   (map (lambda (entry)
          (cons (symbol->string (car entry))
                (json-array
                 (map (lambda (b)
                        (json-array (list (scopes->json (car b))
                                          (json-symbol (cdr b)))))
                      (cdr entry)))))
        binds)))

(define (boxes->json boxes)
  (json-array
   (map (lambda (b)
          (json-array (list (json-symbol (car b))
                            (value->json (cdr b)))))
        boxes)))

(define (def-envs->json def-envs)
  (json-array
   (map (lambda (d)
          (json-array (list (json-symbol (car d))
                            (env->json (cdr d)))))
        def-envs)))

(define (store->json store)
  (json-object
   (list (cons "counter"  (json-number (store-counter store)))
         (cons "binds"    (binds->json (store-binds store)))
         (cons "boxes"    (boxes->json (store-boxes store)))
         (cons "def-envs" (def-envs->json (store-def-envs store))))))

;;; ----------------------------------------
;;; Environments

(define (env->json env)
  (json-array
   (map (lambda (e)
          (json-array (list (json-symbol (car e))
                            (value->json (cdr e)))))
        env)))

;;; ----------------------------------------
;;; Generic values (AST, tagged values, atoms)

;;; Pairs become JSON arrays; a dotted tail is appended as a final element, so
;;; tagged values keep a uniform representation.
(define (list->json x)
  (let loop ((lst x) (acc '()))
    (cond
     ((null? lst) (json-array (reverse acc)))
     ((pair? lst) (loop (cdr lst) (cons (value->json (car lst)) acc)))
     (else (json-array (reverse (cons (value->json lst) acc)))))))

(define (value->json x)
  (cond
   ((stx? x) (stx->json x))
   ((store? x) (store->json x))
   ((null? x) "[]")
   ((pair? x) (list->json x))
   ((symbol? x) (json-symbol x))
   ((number? x) (json-number x))
   ((boolean? x) (if x "true" "false"))
   ((string? x) (json-string x))
   (else (json-string "<opaque>"))))
