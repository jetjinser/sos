;;; defs-model.scm
;;; Guile implementation of the definition-contexts model
;;; (1:1 translation from defs-model.rkt, PLT Redex)
;;; Extends local-model with NEW-DEFS, DEF-BIND, BOX/UNBOX/SET-BOX!.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (defs-model)
  #:use-module (srfi srfi-1)  ; list
  #:use-module (srfi srfi-9)  ; record
  #:use-module (srfi srfi-11) ; values
  #:use-module (ice-9 receive)
  #:use-module (core-model)
  #:use-module (phases-model)
  #:use-module (local-model)
  #:use-module (ssv emit)
  #:export (defs-eval defs-expand))

;;; ----------------------------------------
;;; Σ* triple (reused from local-model)

(define (make-Sigma* store scps-p scps-u)
  (list store scps-p scps-u))

(define (Sigma*-store s*) (car s*))
(define (Sigma*-scps-p s*) (cadr s*))
(define (Sigma*-scps-u s*) (caddr s*))

;;; ----------------------------------------
;;; TStop (reused from local-model)

(define (tstop transform) (cons 'tstop transform))
(define (tstop? x) (and (pair? x) (eq? (car x) 'tstop)))
(define (tstop-transform x) (cdr x))

(define (unstop transform)
  (if (tstop? transform)
      (tstop-transform transform)
      transform))

;;; ----------------------------------------
;;; Defs value: (defs scp . addr)

(define (make-defs scp addr) (cons 'defs (cons scp addr)))
(define (defs-scp x) (cadr x))
(define (defs-addr x) (cddr x))

;;; ----------------------------------------
;;; Box operations

(define (alloc-box store)
  (let ((addr (string->symbol (string-append "bx:"
                               (number->string (store-counter store))))))
    (values addr (make-store (+ (store-counter store) 1)
                             (store-binds store)
                             (store-boxes store)
                             (store-def-envs store)))))

(define (box-lookup store addr)
  (let ((entry (assq addr (store-boxes store))))
    (if entry (cdr entry) (error "box-lookup: not found" addr))))

(define (box-update store addr val)
  (let ((boxes (store-boxes store)))
    (if (assq addr boxes)
        (make-store (store-counter store)
                    (store-binds store)
                    (map (lambda (b)
                           (if (eq? (car b) addr)
                               (cons addr val)
                               b))
                         boxes)
                    (store-def-envs store))
        (make-store (store-counter store)
                    (store-binds store)
                    (cons (cons addr val) boxes)
                    (store-def-envs store)))))

;;; ----------------------------------------
;;; Definition-context environment operations

(define (alloc-def-env store)
  (let ((addr (string->symbol (string-append "env:"
                               (number->string (store-counter store))))))
    (values addr (make-store (+ (store-counter store) 1)
                             (store-binds store)
                             (store-boxes store)
                             (store-def-envs store)))))

(define (def-env-lookup store addr)
  (let ((entry (assq addr (store-def-envs store))))
    (if entry (cdr entry) (error "def-env-lookup: not found" addr))))

(define (def-env-update store addr env)
  (let ((def-envs (store-def-envs store)))
    (if (assq addr def-envs)
        (make-store (store-counter store)
                    (store-binds store)
                    (store-boxes store)
                    (map (lambda (d)
                           (if (eq? (car d) addr)
                               (cons addr env)
                               d))
                         def-envs))
        (make-store (store-counter store)
                    (store-binds store)
                    (store-boxes store)
                    (cons (cons addr env) def-envs)))))

;;; ----------------------------------------
;;; Environment helpers

(define (env-extend* env entries)
  (append entries env))

;;; ----------------------------------------
;;; Eval (with definitions, boxes)

(define (defs-eval ph ast maybe-scp env s*)
  (cond
   ;; NEW-DEFS
   ((and (pair? ast) (eq? (car ast) 'app)
         (eq? (cadr ast) 'NEW-DEFS))
    (let*-values ([(store)       (Sigma*-store s*)]
                  [(scp-defs s1) (alloc-scope (make-stx 'defs '()) store)]
                  [(addr-env s2) (alloc-def-env s1)])
      (let* ([scps-p (Sigma*-scps-p s*)]
             [scps-u (Sigma*-scps-u s*)]
             [s3     (def-env-update s2 addr-env env)]
             [result (make-defs scp-defs addr-env)]
             [s*out  (make-Sigma* s3 (scps-union (list scp-defs) scps-p) scps-u)])
        (emit-rule 'NEW-DEFS ast result s3 env (list (cons 'phase ph)))
        (values result s*out))))

   ;; DEF-BIND (variable: 2 args)
   ((and (pair? ast) (eq? (car ast) 'app)
         (eq? (cadr ast) 'DEF-BIND)
         (= (length ast) 4))
    (let*-values ([(defs-val s*2) (defs-eval ph (caddr ast) maybe-scp env s*)]
                  [(id-arg s*3)   (defs-eval ph (cadddr ast) maybe-scp env s*2)]
                  [(store3)       (Sigma*-store s*3)]
                  [(scps-p3)      (Sigma*-scps-p s*3)]
                  [(scps-u3)      (Sigma*-scps-u s*3)]
                  [(scp-defs)     (defs-scp defs-val)]
                  [(addr-env)     (defs-addr defs-val)]
                  [(id-flipped)   (ph-stx-flip ph id-arg maybe-scp)]
                  [(id-pruned)    (ph-stx-prune ph id-flipped scps-u3)]
                  [(id-defs)      (ph-stx-add ph id-pruned scp-defs)]
                  [(nam-new s4)   (alloc-name id-defs store3)]
                  [(s5)           (ph-store-bind ph s4 id-defs nam-new)]
                  [(env-defs)     (def-env-lookup s5 addr-env)]
                  [(s6)           (def-env-update s5 addr-env
                                    (env-extend env-defs nam-new
                                                (cons 'tvar id-defs)))])
      (emit-rule 'DEF-BIND ast 0 s6 env
                 (list (cons 'phase ph) (cons 'name nam-new) (cons 'kind 'var)))
      (values 0 (make-Sigma* s6 scps-p3 scps-u3))))

   ;; DEF-BIND (macro: 3 args)
   ((and (pair? ast) (eq? (car ast) 'app)
         (eq? (cadr ast) 'DEF-BIND)
         (= (length ast) 5))
    (let*-values ([(defs-val s*2) (defs-eval ph (caddr ast) maybe-scp env s*)]
                  [(id-arg   s*3) (defs-eval ph (cadddr ast) maybe-scp env s*2)]
                  [(stx-arg  s*4) (defs-eval ph (list-ref ast 4) maybe-scp env s*3)]
                  [(store4      ) (Sigma*-store s*4)]
                  [(scps-p4     ) (Sigma*-scps-p s*4)]
                  [(scps-u4     ) (Sigma*-scps-u s*4)]
                  [(scp-defs    ) (defs-scp defs-val)]
                  [(addr-env    ) (defs-addr defs-val)]
                  [(stx-flipped ) (ph-stx-flip ph stx-arg maybe-scp)]
                  [(stx-added   ) (ph-stx-add ph stx-flipped scp-defs)]
                  [(s*rhs       ) (make-Sigma* store4 '() '())]
                  [(stx-exp s*5)  (defs-expand (+ ph 1) stx-added (primitives-env) s*rhs)]
                  [(s5          ) (Sigma*-store s*5)]
                  [(s*eval      ) (make-Sigma* s5 scps-p4 '())]
                  [(val-exp s*6)  (defs-eval ph (ph-parse (+ ph 1) stx-exp s5)
                                             #f env s*eval)]
                  [(s6          ) (Sigma*-store s*6)]
                  [(env-defs    ) (def-env-lookup s6 addr-env)]
                  [(id-flipped  ) (ph-stx-flip ph id-arg maybe-scp)]
                  [(id-pruned   ) (ph-stx-prune ph id-flipped scps-u4)]
                  [(id-defs     ) (ph-stx-add ph id-pruned scp-defs)]
                  [(nam-new  s7 ) (alloc-name id-defs s6)]
                  [(s8          ) (ph-store-bind ph s7 id-defs nam-new)]
                  [(s9          ) (def-env-update s8 addr-env
                                    (env-extend env-defs nam-new val-exp))])
      (emit-rule 'DEF-BIND ast 0 s9 env
                 (list (cons 'phase ph) (cons 'name nam-new) (cons 'kind 'macro)))
      (values 0 (make-Sigma* s9 scps-p4 scps-u4))))

   ;; LOCAL-EXPAND with definition context (3 args)
   ((and (pair? ast) (eq? (car ast) 'app)
         (eq? (cadr ast) 'LOCAL-EXPAND)
         (= (length ast) 5))
    (let*-values ([(stx s*2)       (defs-eval ph (caddr ast) maybe-scp env s*)]
                  [(stop-list s*3) (defs-eval ph (cadddr ast) maybe-scp env s*2)]
                  [(defs-val s*4)  (defs-eval ph (list-ref ast 4) maybe-scp env s*3)]
                  [(store4)        (Sigma*-store s*4)]
                  [(scp-defs)      (defs-scp defs-val)]
                  [(addr-env)      (defs-addr defs-val)]
                  [(env-defs)      (def-env-lookup store4 addr-env)]
                  [(env-unstops)   (map (lambda (entry)
                                          (cons (car entry) (unstop (cdr entry))))
                                        env-defs)]
                  [(stops)         (if (and (pair? stop-list) (eq? (car stop-list) 'list-val))
                                       (cdr stop-list)
                                       '())]
                  [(env-stops)     (env-extend* env-unstops
                                                (map (lambda (id-stop)
                                                       (let ((resolved (ph-resolve ph id-stop store4)))
                                                         (cons resolved
                                                               (tstop (env-lookup env-defs resolved)))))
                                                     stops))]
                  [(stx-flipped)   (ph-stx-flip ph stx maybe-scp)]
                  [(stx-added)     (ph-stx-add ph stx-flipped scp-defs)]
                  [(stx-exp s*5)   (defs-expand ph stx-added env-stops s*4)]
                  [(result)        (ph-stx-flip ph stx-exp maybe-scp)])
      (emit-rule 'LOCAL-EXPAND/DEFS ast result (Sigma*-store s*5) env (list (cons 'phase ph)))
      (values result s*5)))

   ;; BOX
   ((and (pair? ast) (eq? (car ast) 'app)
         (eq? (cadr ast) 'BOX))
    (let*-values ([(val s*2) (defs-eval ph (caddr ast) maybe-scp env s*)]
                  [(store2)  (Sigma*-store s*2)]
                  [(addr s3) (alloc-box store2)]
                  [(s4)      (box-update s3 addr val)])
      (emit-rule 'BOX ast addr s4 env (list (cons 'phase ph)))
      (values addr (make-Sigma* s4 (Sigma*-scps-p s*2) (Sigma*-scps-u s*2)))))

   ;; UNBOX
   ((and (pair? ast) (eq? (car ast) 'app)
         (eq? (cadr ast) 'UNBOX))
    (let*-values ([(addr s*2) (defs-eval ph (caddr ast) maybe-scp env s*)]
                  [(store2)   (Sigma*-store s*2)]
                  [(result)   (box-lookup store2 addr)])
      (emit-rule 'UNBOX ast result store2 env (list (cons 'phase ph)))
      (values result s*2)))

   ;; SET-BOX!
   ((and (pair? ast) (eq? (car ast) 'app)
         (eq? (cadr ast) 'SET-BOX!))
    (let*-values ([(addr s*2) (defs-eval ph (caddr ast) maybe-scp env s*)]
                  [(val  s*3) (defs-eval ph (cadddr ast) maybe-scp env s*2)]
                  [(store3)   (Sigma*-store s*3)]
                  [(s4)       (box-update store3 addr val)])
      (emit-rule 'SET-BOX! ast val s4 env (list (cons 'phase ph)))
      (values val (make-Sigma* s4 (Sigma*-scps-p s*3) (Sigma*-scps-u s*3)))))

   ;; LOCAL-VALUE
   ((and (pair? ast) (eq? (car ast) 'app)
         (eq? (cadr ast) 'LOCAL-VALUE))
    (let*-values ([(id-result s*2) (defs-eval ph (caddr ast) maybe-scp env s*)]
                  [(store2)        (Sigma*-store s*2)]
                  [(result)        (env-lookup env (ph-resolve ph id-result store2))])
      (emit-rule 'LOCAL-VALUE ast result store2 env (list (cons 'phase ph)))
      (values result s*2)))

   ;; LOCAL-EXPAND (2 args, no defs)
   ((and (pair? ast) (eq? (car ast) 'app)
         (eq? (cadr ast) 'LOCAL-EXPAND)
         (= (length ast) 4))
    (let*-values ([(stx       s*2) (defs-eval ph (caddr ast) maybe-scp env s*)]
                  [(stop-list s*3) (defs-eval ph (cadddr ast) maybe-scp env s*2)]
                  [(store3)        (Sigma*-store s*3)]
                  [(env-unstops)   (map (lambda (entry)
                                          (cons (car entry) (unstop (cdr entry))))
                                        env)]
                  [(stops)         (if (and (pair? stop-list) (eq? (car stop-list) 'list-val))
                                       (cdr stop-list)
                                       '())]
                  [(env-stops)     (env-extend* env-unstops
                                                (map (lambda (id-stop)
                                                       (let ((resolved (ph-resolve ph id-stop store3)))
                                                         (cons resolved
                                                               (tstop (env-lookup env resolved)))))
                                                     stops))]
                  [(stx-flipped)   (ph-stx-flip ph stx maybe-scp)]
                  [(stx-exp s*4)   (defs-expand ph stx-flipped env-stops s*3)]
                  [(result)        (ph-stx-flip ph stx-exp maybe-scp)])
      (emit-rule 'LOCAL-EXPAND ast result (Sigma*-store s*4) env (list (cons 'phase ph)))
      (values result s*4)))

   ;; LOCAL-BINDER
   ((and (pair? ast) (eq? (car ast) 'app)
         (eq? (cadr ast) 'LOCAL-BINDER))
    (let*-values ([(id-result s*2) (defs-eval ph (caddr ast) maybe-scp env s*)]
                  [(scps-u2)       (Sigma*-scps-u s*2)]
                  [(result)        (ph-stx-prune ph id-result scps-u2)])
      (emit-rule 'LOCAL-BINDER ast result (Sigma*-store s*2) env (list (cons 'phase ph)))
      (values result s*2)))

   ;; function application
   ((and (pair? ast) (eq? (car ast) 'app)
         (pair? (cadr ast)) (eq? (car (cadr ast)) 'fun))
    (let*-values ([(rator)       (cadr ast)]
                  [(bvar)        (cadr (cadr rator))]
                  [(body)        (caddr rator)]
                  [(val-arg s*2) (defs-eval ph (caddr ast) maybe-scp env s*)]
                  [(result s*3)  (defs-eval ph (subst body bvar val-arg) maybe-scp env s*2)])
      (emit-rule 'fun-app ast result (Sigma*-store s*3) env (list (cons 'phase ph)))
      (values result s*3)))

   ;; primitive application
   ((and (pair? ast) (eq? (car ast) 'app)
         (prim? (cadr ast)))
    (let*-values ([(vals s*2) (defs-eval* ph '() (cddr ast) maybe-scp env s*)]
                  [(result)   (delta (cadr ast) vals)])
      (emit-rule 'prim-app ast result (Sigma*-store s*2) env (list (cons 'phase ph)))
      (values result s*2)))

   ;; value
   (else
    (emit-rule 'value ast ast (Sigma*-store s*) env (list (cons 'phase ph)))
    (values ast s*))))

(define (defs-eval* ph done todo maybe-scp env s*)
  (if (null? todo)
      (values (reverse done) s*)
      (receive (val s*2) (defs-eval ph (car todo) maybe-scp env s*)
        (defs-eval* ph (cons val done) (cdr todo) maybe-scp env s*2))))

;;; ----------------------------------------
;;; Expand (same as local, but calls defs-eval)

(define (defs-expand ph stx env s*)
  (let ((store (Sigma*-store s*))
        (scps-p (Sigma*-scps-p s*))
        (scps-u (Sigma*-scps-u s*))
        (form (stx-form stx)))
    (cond
     ((pair? form)
      (let ((first (car form)))
        (cond
         ;; stops
         ((and (stx? first)
               (tstop? (env-lookup env (ph-resolve ph first store))))
          (emit-rule 'stops stx stx store env (list (cons 'phase ph)))
          (values stx s*))

         ;; lambda
         ((and (stx? first)
               (eq? (ph-resolve ph first store) 'lambda))
          (let*-values ([(id-arg)       (cadr form)]
                        [(body)         (caddr form)]
                        [(nam-new s1)   (alloc-name id-arg store)]
                        [(scp-new s2)   (alloc-scope id-arg s1)]
                        [(id-new)       (ph-stx-add ph id-arg scp-new)]
                        [(s3)           (ph-store-bind ph s2 id-new nam-new)]
                        [(env-new)      (env-extend env nam-new (cons 'tvar id-new))]
                        [(body-added)   (ph-stx-add ph body scp-new)]
                        [(s*inner)      (make-Sigma* s3
                                                     (scps-union (list scp-new) scps-p)
                                                     '())]
                        [(body-exp s*4) (defs-expand ph body-added env-new s*inner)])
            (let ([result (make-stx (list first id-new body-exp) (stx-ctx stx))]
                  [s*out  (make-Sigma* (Sigma*-store s*4) scps-p scps-u)])
              (emit-rule 'lambda stx result (Sigma*-store s*4) env
                         (list (cons 'phase ph) (cons 'name nam-new) (cons 'scope scp-new)))
              (values result s*out))))

         ;; quote
         ((and (stx? first)
               (eq? (ph-resolve ph first store) 'quote))
          (emit-rule 'quote stx stx store env (list (cons 'phase ph)))
          (values stx s*))

         ;; syntax
         ((and (stx? first)
               (eq? (ph-resolve ph first store) 'syntax))
          (let* ((pruned (ph-stx-prune ph (cadr form) scps-p))
                 (result (make-stx (list first pruned) (stx-ctx stx))))
            (emit-rule 'syntax stx result store env (list (cons 'phase ph)))
            (values result s*)))

         ;; let-syntax
         ((and (stx? first)
               (eq? (ph-resolve ph first store) 'let-syntax))
          (let*-values ([(id)          (cadr form)]
                        [(rhs)         (caddr form)]
                        [(body)        (cadddr form)]
                        [(nam-new s1)  (alloc-name id store)]
                        [(scp-new s2)  (alloc-scope id s1)]
                        [(id-new)      (ph-stx-add ph id scp-new)]
                        [(s3)          (ph-store-bind ph s2 id-new nam-new)]
                        [(s*rhs)       (make-Sigma* s3 '() '())]
                        [(stx-exp s*4) (defs-expand (+ ph 1) rhs (primitives-env) s*rhs)]
                        [(s4)          (Sigma*-store s*4)]
                        [(s*eval)      (make-Sigma* s4 scps-p '())]
                        [(val-exp s*5) (defs-eval ph (ph-parse (+ ph 1) stx-exp s4)
                                                  #f env s*eval)]
                        [(s5)          (Sigma*-store s*5)]
                        [(env-new)     (env-extend env nam-new val-exp)]
                        [(body-added)  (ph-stx-add ph body scp-new)]
                        [(s*body)      (make-Sigma* s5
                                                    (scps-union (list scp-new) scps-p)
                                                    '())]
                        [(body-exp s*6) (defs-expand ph body-added env-new s*body)])
            (let ([result body-exp]
                  [s*out  (make-Sigma* (Sigma*-store s*6) scps-p scps-u)])
              (emit-rule 'let-syntax stx result (Sigma*-store s*6) env
                         (list (cons 'phase ph) (cons 'name nam-new) (cons 'scope scp-new)))
              (values result s*out))))

         ;; macro invocation
         ((and (stx? first)
               (let ((t (env-lookup env (ph-resolve ph first store))))
                 (and (not (eq? t (ph-resolve ph first store)))
                      (not (and (pair? t) (eq? (car t) 'tvar))))))
          (let*-values ([(scp-u s1)       (alloc-scope (make-stx 'u '()) store)]
                        [(scp-i s2)       (alloc-scope (make-stx 'i '()) s1)]
                        [(val)            (env-lookup env (ph-resolve ph first store))]
                        [(s*3)            (make-Sigma* s2
                                                       (scps-union (list scp-u) scps-p)
                                                       (scps-union (list scp-u) scps-u))]
                        [(stx-added)      (ph-stx-add ph stx scp-u)]
                        [(stx-flipped)    (ph-stx-flip ph stx-added scp-i)]
                        [(stx-exp s*4)    (defs-eval ph `(app ,val ,stx-flipped)
                                                     scp-i env s*3)]
                        [(result-flipped) (ph-stx-flip ph stx-exp scp-i)]
                        [(expanded s*5)   (defs-expand ph result-flipped env s*4)])
            (emit-rule 'macro-invoke stx expanded (Sigma*-store s*5) env
                       (list (cons 'phase ph) (cons 'scp-u scp-u) (cons 'scp-i scp-i)))
            (values expanded s*5)))

         ;; application
         (else
          (let*-values ([(s*app)              (make-Sigma* store scps-p '())]
                        [(expanded store-out) (defs-expand* ph '() form env s*app)]
                        [(result)             (make-stx expanded (stx-ctx stx))]
                        [(s*out)              (make-Sigma* store-out scps-p scps-u)])
            (emit-rule 'app stx result store-out env (list (cons 'phase ph)))
            (values result s*out))))))

     ;; identifier
     ((stx? stx)
      (let ((transform (env-lookup env (ph-resolve ph stx store))))
        (if (and (pair? transform) (eq? (car transform) 'tvar))
            (begin
              (emit-rule 'id stx (cdr transform) store env
                         (list (cons 'phase ph) (cons 'tvar (cdr transform))))
              (values (cdr transform) s*))
            (begin
              (emit-rule 'id stx stx store env (list (cons 'phase ph)))
              (values stx s*)))))
     (else
      (emit-rule 'literal stx stx store env (list (cons 'phase ph)))
      (values stx s*)))))

(define (defs-expand* ph done todo env s*)
  (if (null? todo)
      (values (reverse done) (Sigma*-store s*))
      (let*-values ([(s*cur)          (make-Sigma* (Sigma*-store s*) (Sigma*-scps-p s*) '())]
                    [(expanded s*out) (defs-expand ph (car todo) env s*cur)])
        (defs-expand* ph (cons expanded done) (cdr todo) env
                      (make-Sigma* (Sigma*-store s*out)
                                   (Sigma*-scps-p s*)
                                   '())))))
