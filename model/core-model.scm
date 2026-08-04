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
            value-of stuck?
            parse parse-tmpl parse-scase eval-tmpl scase-match
            expand
            init-store primitives-env
            register-for-syntax! for-syntax-env))

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
  (memq x '(syntax->datum datum->syntax
            bound-identifier=? free-identifier=? generate-temporaries
            if stx-len =
            + - CONS CAR CDR LIST
            LOCAL-VALUE LOCAL-EXPAND LOCAL-BINDER
            BOX UNBOX SET-BOX! NEW-DEFS DEF-BIND)))

(define (delta prim args)
  (match (cons prim args)
    [('syntax->datum stx)       (stx-form stx)]
    [('datum->syntax id datum)  (make-stx datum (stx-ctx id) #f)]
    [('bound-identifier=? a b) (equal? (stx-ctx a) (stx-ctx b))]
    [('stx-len stx)             (length (stx-form stx))]
    [('CAR (head . _))          head]
    [('CDR (_ . tail))          tail]
    [('+ a b)                   (+ a b)]
    [('- a b)                   (- a b)]
    [('= a b)                   (= a b)]
    [('CONS a b)                (cons a b)]
    [('LIST . elts)             elts]
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
    [('tvar s)
     (if (and (stx? s) (eq? (stx-form s) var)) val ast)]
    [('tlist s . elts)
     `(tlist ,s ,@(map (cut subst <> var val) elts))]
    [('tseq e)
     `(tseq ,(subst e var val))]
    [('tmpl t)
     `(tmpl ,(subst t var val))]
    [('scase subj lits . clauses)
     `(scase ,(subst subj var val) ,lits
             ,@(map (lambda (c) (list (car c) (subst (cadr c) var val)))
                    clauses))]
    [ast ast]))

(define (eval-ast ast store)
  (match ast
    [('app ('fun ('var bvar) body) rand . rands)
     (let*-values ([(val s1)    (eval-ast rand store)]
                   [(result s2) (eval-ast (subst body bvar val) s1)])
       (values result s2))]
    [('app 'free-identifier=? a b)
     (let*-values ([(va s1) (eval-ast a store)]
                   [(vb s2) (eval-ast b s1)])
       (values (eq? (resolve va s2) (resolve vb s2)) s2))]
    [('app 'generate-temporaries lst)
     (let*-values ([(v s1) (eval-ast lst store)])
       (gen-temps v s1))]
    [('app 'if c t e)
     (let*-values ([(cv s1) (eval-ast c store)])
       (if cv
           (eval-ast t s1)
           (eval-ast e s1)))]
    [('app (? prim? rator) . rands)
     (let*-values ([(vals s1) (eval-ast* '() rands store)])
       (values (delta rator vals) s1))]
    [('app (? procedure? rator) . rands)
     (let*-values ([(vals s1) (eval-ast* '() rands store)])
       (values (apply rator vals) s1))]
    [('app rator . rands)
     (error "eval-ast: cannot apply" rator)]
    [('tmpl tast)
     (values (eval-tmpl tast) store)]
    [('scase subj lits . clauses)
     (let*-values ([(s s1) (eval-ast subj store)])
       (scase-match s lits clauses s1 eval-ast (cut resolve <> s1)))]
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

(define (gen-temps stxs store)
  (if (null? stxs)
      (values '() store)
      (let*-values ([(scp s1)    (alloc-scope (car stxs) store)]
                    [(freshened) (stx-add (car stxs) scp)]
                    [(rest s2)   (gen-temps (cdr stxs) s1)])
        (values (cons freshened rest) s2))))

;;; ----------------------------------------
;;; Syntax templates ((syntax ...) / #')
;;;
;;; parse turns (syntax tmpl) into (tmpl <tast>) where the template AST keeps
;;; every source node: identifiers as (tvar <stx>), other atoms as (tlit <stx>),
;;; compounds as (tlist <stx> <elt> ...), and `x ...` as (tseq <elt>).  subst
;;; replaces the tvar positions of pattern variables with their matched stx
;;; values; eval-tmpl then rebuilds the stx tree.  A tvar that survives subst
;;; is a literal identifier of the template.

(define (parse-tmpl stx)
  (let ((form (stx-form stx)))
    (cond
      ((pair? form) `(tlist ,stx ,@(parse-tmpl-list form)))
      ((symbol? form) `(tvar ,stx))
      (else `(tlit ,stx)))))

(define (parse-tmpl-list lst)
  (cond
    ((null? lst) '())
    ((and (pair? (cdr lst))
          (stx? (cadr lst))
          (eq? (stx-form (cadr lst)) '...))
     (cons `(tseq ,(parse-tmpl (car lst)))
           (parse-tmpl-list (cddr lst))))
    (else (cons (parse-tmpl (car lst))
                (parse-tmpl-list (cdr lst))))))

(define (eval-tmpl tast)
  (match tast
    [('tvar s) s]
    [('tlit s) s]
    [('tlist s . elts)
     (make-stx (tmpl-elts elts) (stx-ctx s) (stx-span s))]
    [val val]))

(define (tmpl-elts elts)
  (cond
    ((null? elts) '())
    ((and (pair? (car elts)) (eq? (caar elts) 'tseq))
     (let ((v (eval-tmpl (cadar elts))))
       (append (if (stx? v) (list v) v)
               (tmpl-elts (cdr elts)))))
    (else (cons (eval-tmpl (car elts))
                (tmpl-elts (cdr elts))))))

;;; ----------------------------------------
;;; syntax-case
;;;
;;; parse turns (syntax-case subj (lit ...) clause ...) into
;;; (scase <subj-ast> <lit-stx ...> (<pattern-stx> <body-ast>) ...): the
;;; subject and clause bodies are parsed upfront (pattern variables become
;;; (var ...) references, templates become tmpl ASTs), while patterns and
;;; literals stay raw stx for the matcher.  On a match the bound pattern
;;; variables are subst-ed into the body before evaluation, which keeps the
;;; substitution-based eval (and nested syntax-case) working unchanged.

(define (parse-scase parse-fn form)
  `(scase ,(parse-fn (cadr form))
          ,(stx-form (caddr form))
          ,@(map (lambda (clause)
                   (let ((f (stx-form clause)))
                     (list (car f) (parse-fn (cadr f)))))
                 (cdddr form))))

;;; (scase-match s lits clauses store eval-fn resolve-fn): try each clause in
;;; order; on the first match, substitute the bindings into the body and
;;; evaluate it with EVAL-FN.  RESOLVE-FN maps an identifier stx to its name
;;; (literal comparison is free-identifier=? semantics).
(define (scase-match s lits clauses store eval-fn resolve-fn)
  (if (null? clauses)
      (error "syntax-case: no matching clause")
      (let ((bindings (match-pattern s (caar clauses) lits resolve-fn)))
        (if bindings
            (eval-fn (apply-subst (cadar clauses) bindings) store)
            (scase-match s lits (cdr clauses) store eval-fn resolve-fn)))))

(define (apply-subst ast bindings)
  (if (null? bindings)
      ast
      (apply-subst (subst ast (caar bindings) (cdar bindings))
                   (cdr bindings))))

;;; Match S against PAT, returning an alist of pattern variable -> stx, or #f.
;;; `_` is a wildcard; a literal matches by resolved name; a trailing (x ...)
;;; in a list pattern binds x to the remaining suffix.
(define (match-pattern s pat lits resolve-fn)
  (let ((p (stx-form pat)))
    (cond
      ((eq? p '_) '())
      ((lit-stx p lits) =>
       (lambda (lit) (and (eq? (resolve-fn s) (resolve-fn lit)) '())))
      ((symbol? p) (list (cons p s)))
      ((pair? p)
       (let ((f (stx-form s)))
         (and (pair? f) (match-list f p lits resolve-fn))))
      (else (and (equal? p (stx-form s)) '())))))

(define (lit-stx p lits)
  (cond
    ((null? lits) #f)
    ((eq? (stx-form (car lits)) p) (car lits))
    (else (lit-stx p (cdr lits)))))

(define (match-list fs ps lits resolve-fn)
  (let ((te (trailing-ellipsis-ps ps)))
    (if te
        (let ((n (cdr te)))
          (and (>= (length fs) n)
               (let ((b (match-elems (take-stx fs n) (take-stx ps n) lits resolve-fn)))
                 (and b (append b (list (cons (car te) (drop-stx fs n))))))))
        (and (= (length fs) (length ps))
             (match-elems fs ps lits resolve-fn)))))

(define (match-elems fs ps lits resolve-fn)
  (if (null? fs)
      '()
      (let ((b (match-pattern (car fs) (car ps) lits resolve-fn)))
        (and b
             (let ((r (match-elems (cdr fs) (cdr ps) lits resolve-fn)))
               (and r (append b r)))))))

;;; If the pattern list ends with (var ...), return (var . index-of-var).
;;; Only a trailing ellipsis is recognized (nested ellipsis is not supported).
(define (trailing-ellipsis-ps ps)
  (let loop ((lst ps) (idx 0))
    (cond
      ((or (null? lst) (null? (cdr lst))) #f)
      ((and (symbol? (stx-form (car lst)))
            (eq? (stx-form (cadr lst)) '...))
       (cons (stx-form (car lst)) idx))
      ((pair? (stx-form (car lst))) #f)
      (else (loop (cdr lst) (+ idx 1))))))

(define (take-stx lst n)
  (if (= n 0) '() (cons (car lst) (take-stx (cdr lst) (- n 1)))))

(define (drop-stx lst n)
  (if (= n 0) lst (drop-stx (cdr lst) (- n 1))))

;;; ----------------------------------------
;;; Total evaluator (for displaying the reduced result)

;;; Unlike eval-ast this never raises: a term that cannot be reduced to a value
;;; (free variable, non-applicable operator) yields *stuck*.  It is only used
;;; to show the value next to the expansion, never during expansion itself.
(define *stuck* (list '*stuck*))

(define (stuck? x) (eq? x *stuck*))

(define (safe-delta prim vals)
  (match (cons prim vals)
    [('CAR (head . _))                  head]
    [('CDR (_ . tail))                  tail]
    [('+ (? number? a) (? number? b))   (+ a b)]
    [('- (? number? a) (? number? b))   (- a b)]
    [('CONS a b)                        (cons a b)]
    [('LIST . elts)                     elts]
    [_ *stuck*]))

(define (value-of ast)
  (match ast
    [('app ('fun ('var bvar) body) rand . rands)
     (let ((v (value-of rand)))
       (if (stuck? v)
           *stuck*
           (value-of (subst body bvar v))))]
    [('app (? prim? rator) . rands)
     (let ((vals (map value-of rands)))
       (if (any stuck? vals)
           *stuck*
           (safe-delta rator vals)))]
    [('app rator . rands) *stuck*]
    [('fun . rest) ast]
    [('tmpl t) ast]
    [('var v) *stuck*]
    [('list-val lv)
      (let ((vals (map value-of lv)))
        (if (any stuck? vals) *stuck* `(list-val ,@vals)))]
    [else ast]))

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
    [((? (stx-is? 'syntax)) body) `(tmpl ,(parse-tmpl body))]
    [(? (lambda (f)
          (and (pair? f) ((stx-is? 'syntax-case) (car f))))
        form)
     (parse-scase (lambda (s) (parse s store)) form)]
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
                   [(rhs-exp s-rhs)  (expand rhs (for-syntax-env) s3)]
                   [(transformer s4) (eval-ast (parse rhs-exp s-rhs) s-rhs)]
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

;;; ----------------------------------------
;;; For-syntax transformer registry
;;; Library macros (e.g. syntax-rules) register themselves here; let-syntax
;;; expands a transformer's rhs in this environment.

(define *for-syntax-env* '())

(define (register-for-syntax! name transformer)
  (if (not (assq name *for-syntax-env*))
      (set! *for-syntax-env* (cons (cons name transformer) *for-syntax-env*))))

(define (for-syntax-env)
  (append *for-syntax-env* (primitives-env)))
