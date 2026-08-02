;;; core-model.scm
;;; Guile implementation of the single-phase scope-set model
;;; (1:1 translation from core-model.rkt, PLT Redex)
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (core-model)
  #:use-module (srfi srfi-1)  ; list
  #:use-module (srfi srfi-9)  ; record
  #:use-module (srfi srfi-11) ; values
  #:use-module (srfi srfi-26) ; cut(e)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ssv emit)
  #:export (make-stx stx? stx-form stx-ctx stx-span
            make-store store? store-counter store-binds store-boxes store-def-envs
            scps-union scps-subtract scps-addremove scps-subset? biggest-subset
            stx-add stx-flip stx-strip
            store-lookup store-bind binding-lookup resolve
            env-lookup env-extend
            alloc-name alloc-scope
            prim? delta
            subst eval-ast
            parse
            expand
            init-store primitives-env))

;;; ----------------------------------------
;;; Data structures

(define-record-type <stx>
  (make-stx form ctx span)
  stx?
  (form stx-form)
  (ctx stx-ctx)
  (span stx-span))

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
  (cond [(null? s2) s1]
        [(memq (car s2) s1)
         (scps-subtract (delq (car s2) s1) (cdr s2))]
        [else (scps-subtract s1 (cdr s2))]))

(define (scps-addremove scp s)
  (if (memq scp s)
    (delq scp s)
    (cons scp s)))

(define (scps-subset? s1 s2)
  (or (null? s1)
      (and (memq (car s1) s2)
           (scps-subset? (cdr s1) s2))))

(define (biggest-subset ref candidates)
  (let* ([matching (filter (cut scps-subset? <> ref) candidates)]
         [sorted   (sort matching (lambda (a b) (> (length a) (length b))))])
    (if (or (null? sorted)
            (and (pair? (cdr sorted))
                 (= (length (car sorted)) (length (cadr sorted))))
            (any (lambda (b) (not (scps-subset? b (car sorted))))
                 (cdr sorted)))
        #f
        (car sorted))))

;;; ----------------------------------------
;;; Syntax object operations

(define (%stx-add stx scp)
  (let ((form (stx-form stx)))
    (make-stx (if (pair? form)
                  (map (cut %stx-add <> scp) form)
                  form)
              (scps-union (list scp) (stx-ctx stx))
              (stx-span stx))))

(define (stx-add stx scp)
  (let ((r (%stx-add stx scp)))
    (emit-op 'stx-add scp stx r)
    r))

(define (%stx-flip stx scp)
  (let ([form (stx-form stx)])
    (make-stx (if (pair? form)
                (map (cut %stx-flip <> scp) form)
                form)
              (scps-addremove scp (stx-ctx stx))
              (stx-span stx))))

(define (stx-flip stx scp)
  (let ([r (%stx-flip stx scp)])
    (emit-op 'stx-flip scp stx r)
    r))

(define (stx-strip stx)
  (let ((form (stx-form stx)))
    (if (pair? form)
      (cons 'list-val (map stx-strip form))
      form)))

(define (%store-stx-is? store sym)
  (lambda (x)
    (and (stx? x)
         (eq? (resolve x store) sym))))

;;; ----------------------------------------
;;; Store operations

(define (store-lookup store name)
  (let ((entry (assq name (store-binds store))))
    (if entry (cdr entry) '())))

(define (store-bind store id name)
  (let* ([sym      (stx-form id)]
         [scopes   (stx-ctx id)]
         [binds    (store-binds store)]
          [existing (assq sym binds)])
    (emit-op 'bind sym scopes name (stx-span id))
    (if existing
        (make-store (store-counter store)
                    (map (match-lambda
                           [(and (b . bs) bind)
                            (if (eq? b sym)
                              `(,sym . ((,scopes . ,name) . ,bs))
                              bind)])
                         binds)
                    (store-boxes store)
                    (store-def-envs store))
        (make-store (store-counter store)
                    (cons (list sym (cons scopes name)) binds)
                    (store-boxes store)
                    (store-def-envs store)))))

(define (binding-lookup bindings scps)
  (let loop ((bs bindings))
    (match bs
      [()                                       #f]
      [(((? (cut equal? <> scps)) . found) . _) found]
      [(b . bs)                                 (loop bs)])))

(define (resolve id store)
  (let* ([sym      (stx-form id)]
         [ctx      (stx-ctx id)]
         [bindings (store-lookup store sym)])
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
    (emit-op 'alloc-name name)
    (values name (make-store (+ (store-counter store) 1)
                             (store-binds store)
                             (store-boxes store)
                             (store-def-envs store)))))

(define (alloc-scope id store)
  (let ((name (string->symbol (string-append
                               (symbol->string (stx-form id))
                               ":"
                               (number->string (store-counter store))))))
    (emit-op 'alloc-scope name)
    (values name (make-store (+ (store-counter store) 1)
                             (store-binds store)
                             (store-boxes store)
                             (store-def-envs store)))))

;;; ----------------------------------------
;;; Primitives (δ)

(define (prim? x)
  (memq x '(syntax->datum datum->syntax + - CONS CAR CDR LIST
            LOCAL-VALUE LOCAL-EXPAND LOCAL-BINDER
            BOX UNBOX SET-BOX! NEW-DEFS DEF-BIND)))

(define (delta prim args)
  (match (cons prim args)
    [('syntax->datum stx)      (stx-form stx)]
    [('datum->syntax id datum) (make-stx datum (stx-ctx id) #f)]
    [('CAR (head . _))         head]
    [('CDR (_ . tail))         tail]
    [('+ a b)                  (+ a b)]
    [('- a b)                  (- a b)]
    [('CONS a b)               (cons a b)]
    [('LIST . elts)            elts]
    [_ (error "delta: unknown primitive or bad arity" prim args)]))

;;; ----------------------------------------
;;; Substitution and evaluation

(define %fresh-var
  (let ([counter 0])
    (lambda (base)
      (let ([v (format #f "~s#s~a" base counter)])
        (set! counter (+ counter 1))
        (string->symbol v)))))

(define (subst ast var val)
  (match ast
    [('var v)
     (if (eq? v var)
       val
       ast)]
    [('app . args)
     `(app ,@(map (cut subst <> var val) args))]
    [('fun ('var (? (cut eq? <> var))) body)
     ast]
    [('fun ('var bvar) body)
     (let ([fv (%fresh-var bvar)])
       `(fun (var ,fv)
             ,(subst (subst body bvar `(var ,fv)) var val)))]
    [('list-val . elts)
     `(list-val ,@(map (cut subst <> var val) elts))]
    [ast ast]))

(define (eval-ast ast store)
  (match ast
    [('app ('fun ('var bvar) body) rand . rands)
     (let*-values ([(val s1)    (eval-ast rand store)]
                   [(result s2) (eval-ast (subst body bvar val) s1)])
       (values result s2))]
    [('app (? prim? rator) . rands)
     (let*-values ([(vals s1) (eval-ast* '() rands store)])
       (values (delta rator vals) s1))]
    [('app rator . rands)
     (error "eval-ast: cannot apply" rator)]
    [('fun . rest) (values ast store)]
    [('var v)
     (error "eval-ast: unbound variable" v)]
    [('list-val lv)
     (let*-values ([(vals s1) (eval-ast* '() lv store)])
       (values `(list-val ,@vals) s1))]
    [ast (values ast store)]))

(define (eval-ast* done todo store)
  (if (null? todo)
      (values (reverse done) store)
      (let*-values ([(val s1) (eval-ast (car todo) store)])
        (eval-ast* (cons val done) (cdr todo) s1))))

;;; ----------------------------------------
;;; Parse

(define (parse stx store)
  (define (stx-is? sym)
    (%store-stx-is? store sym))

  (match (stx-form stx)
    [((? (stx-is? 'lambda)) id-arg body)
     `(fun (var ,(resolve id-arg store))
           ,(parse body store))]
    [((? (stx-is? 'quote)) body)  (stx-strip body)]
    [((? (stx-is? 'syntax)) body) body]
    [(and (first . rest) form)
     (cons 'app (map (cut parse <> store) form))]
    [(and (or (? number?) (? prim?)) form) form]
    [form `(var ,(resolve stx store))]))

;;; ----------------------------------------
;;; Expand

(define (expand stx env store)
  (define (stx-is? sym)
    (%store-stx-is? store sym))
  (define (shadowed-stx? x)
    (and (stx? x)
         (not (eq? (env-lookup env (resolve x store))
                   (resolve x store)))))

  (match stx
    ;;; compound form
    ;; lambda
    [($ <stx> ((? (stx-is? 'lambda) lam) id-arg body))
     (let*-values ([(nam-new s1)  (alloc-name  id-arg store)]
                   [(scp-new s2)  (alloc-scope id-arg s1)]
                   [(id-new)      (stx-add id-arg scp-new)]
                   [(s3)          (store-bind s2 id-new nam-new)]
                   [(env-new)     (env-extend env nam-new (cons 'tvar id-new))]
                   [(body-added)  (stx-add body scp-new)]
                   [(body-exp s4) (expand body-added env-new s3)])
       (let ([result (make-stx (list lam id-new body-exp) (stx-ctx stx) (stx-span stx))])
         (emit-rule 'lambda stx result s4 env
                    (list (cons 'name nam-new) (cons 'scope scp-new)))
         (values result s4)))]
    ;; quote
    [($ <stx> ((? (stx-is? 'quote) quo) . rest))
     (emit-rule 'quote stx stx store env '())
     (values stx store)]
    ;; syntax
    [($ <stx> ((? (stx-is? 'syntax) syn) . rest))
     (emit-rule 'syntax stx stx store env '())
     (values stx store)]
    ;; let-syntax
    [($ <stx> ((? (stx-is? 'let-syntax) ls) id rhs body))
     (let*-values ([(nam-new s1)     (alloc-name  id store)]
                   [(scp-new s2)     (alloc-scope id s1)]
                   [(id-new)         (stx-add id scp-new)]
                   [(s3)             (store-bind s2 id-new nam-new)]
                   [(transformer s4) (eval-ast (parse rhs s3) s3)]
                   [(env-new)        (env-extend env nam-new transformer)]
                   [(body-added)     (stx-add body scp-new)]
                   [(body-exp s5)    (expand body-added env-new s4)])
       (emit-rule 'let-syntax stx body-exp s5 env
                  (list (cons 'name nam-new) (cons 'scope scp-new)))
       (values body-exp s5))]
    ;; macro invocation
    [($ <stx> ((? shadowed-stx? first) . rest))
     (let*-values ([(scp-u s1)       (alloc-scope (make-stx 'a '() #f) store)]
                   [(scp-i s2)       (alloc-scope (make-stx 'a '() #f) s1)]
                   [(val)            (env-lookup env (resolve first store))]
                   [(stx-added)      (stx-add stx scp-u)]
                   [(stx-flipped)    (stx-flip stx-added scp-i)]
                   [(result s3)      (eval-ast `(app ,val ,stx-flipped) s2)]
                   [(result-flipped) (stx-flip result scp-i)]
                   [(expanded s4)    (expand result-flipped env s3)])
       (emit-rule 'macro-invoke stx expanded s4 env
                  (list (cons 'scp-u scp-u) (cons 'scp-i scp-i)))
       (values expanded s4))]
    ;; application
    [($ <stx> (and (first . rest) form) ctx)
     (let*-values ([(expanded s1) (expand* '() form env store)]
                   [(result)      (make-stx expanded ctx (stx-span stx))])
       (emit-rule 'app stx result s1 env '())
       (values result s1))]
    ;;; identifier
    [($ <stx> form ctx)
     (let ((transform (env-lookup env (resolve stx store))))
       (if (and (pair? transform) (eq? (car transform) 'tvar))
         (begin
           (emit-rule 'id stx (cdr transform) store env
                      (list (cons 'tvar (cdr transform))))
           (values (cdr transform) store))
         (begin
           (emit-rule 'id stx stx store env '())
           (values stx store))))]
    [_ (values stx store)]))

(define (expand* done todo env store)
  (if (null? todo)
    (values (reverse done) store)
    (receive (expanded s1) (expand (car todo) env store)
      (expand* (cons expanded done) (cdr todo) env s1))))

;;; ----------------------------------------
;;; Helpers

(define (init-store)
  (make-store 0 '() '() '()))

(define (primitives-env)
  '())
