;;; tests/unit/ssv/serialize.scm
;;; Test the JSON serializers by parsing their output back with guile-json and
;;; comparing the resulting structure.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (tests unit ssv serialize)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (core-model)
  #:use-module (ssv serialize)
  #:use-module (ssv source)
  #:use-module (json))

(test-begin "ssv-serialize")

;;; guile-json reads arrays as vectors and objects as reverse-keyed alists;
;;; normalize both away so expectations read naturally and ignore key order.
(define (normalize x)
  (cond
   ((vector? x)
    (map normalize (vector->list x)))
   ((and (pair? x) (every (lambda (e) (and (pair? e) (string? (car e)))) x))
    (sort (map (lambda (e) (cons (car e) (normalize (cdr e)))) x)
          (lambda (a b) (string<? (car a) (car b)))))
   ((pair? x)
    (map normalize x))
   (else x)))

(define (parsed s)
  (normalize (json-string->scm s)))

;;; JSON primitives, round-tripped through the parser
(test-equal "json-string" "hi"       (parsed (json-string "hi")))
(test-equal "json-escape" "a\"b\\c"  (parsed (json-string "a\"b\\c")))

;;; Contexts: a core scope set vs a phases phase-map
(test-equal "ctx-core"  '("z:1" "x:3")             (parsed (ctx->json '(z:1 x:3))))
(test-equal "ctx-phase" '((0 ("z:1")) (1 ("a:2"))) (parsed (ctx->json '((0 z:1) (1 a:2)))))
(test-equal "ctx-empty" '()                        (parsed (ctx->json '())))

;;; Generic and tagged values
(test-equal "value-var"  '("var" "z:0")                   (parsed (value->json '(var z:0))))
(test-equal "value-num"  42                               (parsed (value->json 42)))
(test-equal "value-bool" #t                               (parsed (value->json #t)))
(test-equal "value-fun"  '("fun" ("var" "a") ("var" "a")) (parsed (value->json '(fun (var a) (var a)))))

;;; Syntax objects
(test-equal "stx-atom"
  '(("ctx") ("form" . "x") ("span" 0 1))
  (parsed (stx->json (string->stx "x"))))
(test-equal "stx-compound"
  '(("ctx") ("form" (("ctx") ("form" . "a") ("span" 1 2))
                    (("ctx") ("form" . "b") ("span" 3 4)))
    ("span" 0 5))
  (parsed (stx->json (string->stx "(a b)"))))

;;; Store
(test-equal "store-init"
  '(("binds") ("boxes") ("counter" . 0) ("def-envs"))
  (parsed (store->json (init-store))))
(test-equal "store-bound"
  '(("binds" ("z" (("z:1") "z:0"))) ("boxes") ("counter" . 0) ("def-envs"))
  (parsed (store->json (store-bind (init-store) (make-stx 'z '(z:1) #f) 'z:0))))

;;; Environment
(test-equal "env"
  '(("a:4" (("ctx") ("form" . "z") ("span" 0 1)))
    ("m:6" ("fun" ("var" "s") ("var" "s"))))
  (parsed (env->json (list (cons 'a:4 (string->stx "z"))
                           (cons 'm:6 '(fun (var s) (var s)))))))

(test-end "ssv-serialize")
