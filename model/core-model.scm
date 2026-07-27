;;; core-model.scm
;;; Guile implementation of the single-phase scope-set model
;;; (1:1 translation from core-model.rkt, PLT Redex)
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (core-model)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:export (make-stx stx? stx-form stx-ctx
            make-store store? store-counter store-binds store-boxes store-def-envs
            scps-union scps-subtract scps-addremove scps-subset? biggest-subset
            stx-add stx-flip stx-strip
            store-lookup store-bind binding-lookup resolve
            env-lookup env-extend
            alloc-name alloc-scope
            prim? delta
            subst eval-ast
            parse
            expand expand*
            as-syntax init-store primitives-env
            stx->datum))

;;; ----------------------------------------
;;; Data structures

(define-record-type <stx>
  (make-stx form ctx)
  stx?
  (form stx-form)
  (ctx stx-ctx))

(define-record-type <store>
  (make-store counter binds boxes def-envs)
  store?
  (counter store-counter)
  (binds store-binds)
  (boxes store-boxes)
  (def-envs store-def-envs))

;;; ----------------------------------------
;;; Set operations (scope sets)

(define (scps-union s1 s2)
  (append s1 s2))

(define (scps-subtract s1 s2)
  (cond ((null? s2) s1)
        ((memq (car s2) s1)
         (scps-subtract (delq (car s2) s1) (cdr s2)))
        (else (scps-subtract s1 (cdr s2)))))

(define (scps-addremove scp s)
  (if (memq scp s)
      (delq scp s)
      (cons scp s)))

(define (scps-subset? s1 s2)
  (or (null? s1)
      (and (memq (car s1) s2)
           (scps-subset? (cdr s1) s2))))

(define (biggest-subset ref candidates)
  (let* ((matching (filter (lambda (c) (scps-subset? c ref)) candidates))
         (sorted (sort matching (lambda (a b) (> (length a) (length b))))))
    (if (or (null? sorted)
            (and (pair? (cdr sorted))
                 (= (length (car sorted)) (length (cadr sorted))))
            (any (lambda (b) (not (scps-subset? b (car sorted))))
                 (cdr sorted)))
        #f
        (car sorted))))

;;; ----------------------------------------
;;; Syntax object operations

(define (stx-add stx scp)
  (let ((form (stx-form stx)))
    (make-stx (if (pair? form)
                  (map (lambda (s) (stx-add s scp)) form)
                  form)
              (scps-union (list scp) (stx-ctx stx)))))

(define (stx-flip stx scp)
  (let ((form (stx-form stx)))
    (make-stx (if (pair? form)
                  (map (lambda (s) (stx-flip s scp)) form)
                  form)
              (scps-addremove scp (stx-ctx stx)))))

(define (stx-strip stx)
  (let ((form (stx-form stx)))
    (if (pair? form)
        (cons 'list-val (map stx-strip form))
        form)))

;;; ----------------------------------------
;;; Store operations

(define (store-lookup store name)
  (let ((entry (assq name (store-binds store))))
    (if entry (cdr entry) '())))

(define (store-bind store id name)
  (let* ((sym (stx-form id))
         (scopes (stx-ctx id))
         (binds (store-binds store))
         (existing (assq sym binds)))
    (if existing
        (make-store (store-counter store)
                    (map (lambda (b)
                           (if (eq? (car b) sym)
                               (cons sym (cons (cons scopes name) (cdr b)))
                               b))
                         binds)
                    (store-boxes store)
                    (store-def-envs store))
        (make-store (store-counter store)
                    (cons (list sym (cons scopes name)) binds)
                    (store-boxes store)
                    (store-def-envs store)))))

(define (binding-lookup bindings scps)
  (let loop ((bs bindings))
    (cond ((null? bs) #f)
          ((equal? (caar bs) scps) (cdar bs))
          (else (loop (cdr bs))))))

(define (resolve id store)
  (let* ((sym (stx-form id))
         (ctx (stx-ctx id))
         (bindings (store-lookup store sym)))
    (if (null? bindings)
        sym
        (let ((biggest (biggest-subset ctx (map car bindings))))
          (if biggest
              (or (binding-lookup bindings biggest) sym)
              sym)))))

;;; ----------------------------------------
;;; Environment operations

(define (env-lookup env name)
  (let ((entry (assq name env)))
    (if entry (cdr entry) name)))

(define (env-extend env name transform)
  (cons (cons name transform) env))

;;; ----------------------------------------
;;; Allocation

(define (alloc-name id store)
  (let ((name (string->symbol (string-append
                               (symbol->string (stx-form id))
                               ":"
                               (number->string (store-counter store))))))
    (cons name (make-store (+ (store-counter store) 1)
                           (store-binds store)
                           (store-boxes store)
                           (store-def-envs store)))))

(define (alloc-scope id store)
  (let ((name (string->symbol (string-append
                               (symbol->string (stx-form id))
                               ":"
                               (number->string (store-counter store))))))
    (cons name (make-store (+ (store-counter store) 1)
                           (store-binds store)
                           (store-boxes store)
                           (store-def-envs store)))))

;;; ----------------------------------------
;;; Primitives (δ)

(define (prim? x)
  (memq x '(SE MKS + - CONS CAR CDR LIST
            LOCAL-VALUE LOCAL-EXPAND LOCAL-BINDER
            BOX UNBOX SET-BOX! NEW-DEFS DEF-BIND)))

(define (delta prim args)
  (case prim
    ((SE) (stx-form (car args)))
    ((MKS) (let ((val (car args))
                 (ctx-stx (cadr args)))
             (make-stx val (stx-ctx ctx-stx))))
    ((+) (+ (car args) (cadr args)))
    ((-) (- (car args) (cadr args)))
    ((CONS) (cons (car args) (cadr args)))
    ((CAR) (car (car args)))
    ((CDR) (cdr (car args)))
    ((LIST) args)
    (else (error "delta: unknown primitive" prim))))

;;; ----------------------------------------
;;; Substitution and evaluation

(define *subst-counter* 0)

(define (fresh-var base)
  (let ((v (string->symbol (string-append (symbol->string base)
                                          "#s"
                                          (number->string *subst-counter*)))))
    (set! *subst-counter* (+ *subst-counter* 1))
    v))

(define (subst ast var val)
  (cond
   ((and (pair? ast) (eq? (car ast) 'var))
    (if (eq? (cadr ast) var)
        val
        ast))
   ((and (pair? ast) (eq? (car ast) 'app))
    (cons 'app (map (lambda (a) (subst a var val)) (cdr ast))))
   ((and (pair? ast) (eq? (car ast) 'fun))
    (let ((bvar (cadr (cadr ast)))
          (body (caddr ast)))
      (if (eq? bvar var)
          ast
          (let ((fv (fresh-var bvar)))
            `(fun (var ,fv)
                  ,(subst (subst body bvar `(var ,fv)) var val))))))
   ((and (pair? ast) (eq? (car ast) 'list-val))
    (cons 'list-val (map (lambda (v) (subst v var val)) (cdr ast))))
   ((stx? ast) ast)
   (else ast)))

(define (eval-ast ast)
  (cond
   ((and (pair? ast) (eq? (car ast) 'app))
    (let ((rator (cadr ast))
          (rands (cddr ast)))
      (cond
       ((and (pair? rator) (eq? (car rator) 'fun))
        (let ((bvar (cadr (cadr rator)))
              (body (caddr rator)))
          (eval-ast (subst body bvar (eval-ast (car rands))))))
       ((prim? rator)
        (delta rator (map eval-ast rands)))
       (else
        (error "eval-ast: cannot apply" rator)))))
   ((and (pair? ast) (eq? (car ast) 'fun)) ast)
   ((and (pair? ast) (eq? (car ast) 'var))
    (error "eval-ast: unbound variable" (cadr ast)))
   ((and (pair? ast) (eq? (car ast) 'list-val))
    (cons 'list-val (map eval-ast (cdr ast))))
   (else ast)))

;;; ----------------------------------------
;;; Parse

(define (parse stx store)
  (let ((form (stx-form stx)))
    (cond
     ((pair? form)
      (let ((first (car form)))
        (cond
         ((and (stx? first)
               (eq? (resolve first store) 'lambda))
          (let ((id-arg (cadr form))
                (body (caddr form)))
            `(fun (var ,(resolve id-arg store))
                  ,(parse body store))))
         ((and (stx? first)
               (eq? (resolve first store) 'quote))
          (stx-strip (cadr form)))
         ((and (stx? first)
               (eq? (resolve first store) 'syntax))
          (cadr form))
         (else
          (cons 'app (map (lambda (s) (parse s store)) form))))))
     ((or (number? form) (prim? form)) form)
     (else `(var ,(resolve stx store))))))

;;; ----------------------------------------
;;; Expand

(define (expand stx env store)
  (let ((form (stx-form stx)))
    (cond
     ;; compound form
     ((pair? form)
      (let ((first (car form)))
        (cond
         ;; lambda
         ((and (stx? first)
               (eq? (resolve first store) 'lambda))
          (let* ((id-arg (cadr form))
                 (body (caddr form))
                 (an (alloc-name id-arg store))
                 (nam-new (car an))
                 (s1 (cdr an))
                 (as (alloc-scope id-arg s1))
                 (scp-new (car as))
                 (s2 (cdr as))
                 (id-new (stx-add id-arg scp-new))
                 (s3 (store-bind s2 id-new nam-new))
                 (env-new (env-extend env nam-new (cons 'tvar id-new)))
                 (body-added (stx-add body scp-new))
                 (er (expand body-added env-new s3)))
            (cons (make-stx (list first id-new (car er)) (stx-ctx stx))
                  (cdr er))))
         ;; quote
         ((and (stx? first)
               (eq? (resolve first store) 'quote))
          (cons stx store))
         ;; syntax
         ((and (stx? first)
               (eq? (resolve first store) 'syntax))
          (cons stx store))
         ;; let-syntax
         ((and (stx? first)
               (eq? (resolve first store) 'let-syntax))
          (let* ((id (cadr form))
                 (rhs (caddr form))
                 (body (cadddr form))
                 (an (alloc-name id store))
                 (nam-new (car an))
                 (s1 (cdr an))
                 (as (alloc-scope id s1))
                 (scp-new (car as))
                 (s2 (cdr as))
                 (id-new (stx-add id scp-new))
                 (s3 (store-bind s2 id-new nam-new))
                 (transformer (eval-ast (parse rhs s3)))
                 (env-new (env-extend env nam-new transformer))
                 (body-added (stx-add body scp-new)))
            (expand body-added env-new s3)))
         ;; macro invocation
         ((and (stx? first)
               (not (eq? (env-lookup env (resolve first store))
                         (resolve first store))))
          (let* ((as1 (alloc-scope (make-stx 'a '()) store))
                 (scp-u (car as1))
                 (s1 (cdr as1))
                 (as2 (alloc-scope (make-stx 'a '()) s1))
                 (scp-i (car as2))
                 (s2 (cdr as2))
                 (val (env-lookup env (resolve first store)))
                 (stx-added (stx-add stx scp-u))
                 (stx-flipped (stx-flip stx-added scp-i))
                 (result (eval-ast `(app ,val ,stx-flipped)))
                 (result-flipped (stx-flip result scp-i)))
            (expand result-flipped env s2)))
         ;; application
         (else
          (let ((er (expand* '() form env store)))
            (cons (make-stx (car er) (stx-ctx stx)) (cdr er)))))))
     ;; identifier
     ((stx? stx)
      (let ((transform (env-lookup env (resolve stx store))))
        (if (and (pair? transform) (eq? (car transform) 'tvar))
            (cons (cdr transform) store)
            (cons stx store))))
     (else (cons stx store)))))

(define (expand* done todo env store)
  (if (null? todo)
      (cons (reverse done) store)
      (let ((er (expand (car todo) env store)))
        (expand* (cons (car er) done) (cdr todo) env (cdr er)))))

;;; ----------------------------------------
;;; Helpers

(define (as-syntax datum)
  (cond
   ((pair? datum)
    (make-stx (map as-syntax datum) '()))
   (else
    (make-stx datum '()))))

(define (init-store)
  (make-store 0 '() '() '()))

(define (primitives-env)
  '())

;;; ----------------------------------------
;;; Display helpers

(define (stx->datum x)
  (cond
   ((stx? x) (stx->datum (stx-form x)))
   ((pair? x) (map stx->datum x))
   (else x)))
