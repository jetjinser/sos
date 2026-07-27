;;; defs-model.scm
;;; Guile implementation of the definition-contexts model
;;; (1:1 translation from defs-model.rkt, PLT Redex)
;;; Extends local-model with NEW-DEFS, DEF-BIND, BOX/UNBOX/SET-BOX!.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (defs-model)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (core-model)
  #:use-module (phases-model)
  #:use-module (local-model)
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
(define (defs? x) (and (pair? x) (eq? (car x) 'defs)))
(define (defs-scp x) (cadr x))
(define (defs-addr x) (cddr x))

;;; ----------------------------------------
;;; Box operations

(define (alloc-box store)
  (let ((addr (string->symbol (string-append "bx:"
                               (number->string (store-counter store))))))
    (cons addr (make-store (+ (store-counter store) 1)
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
    (cons addr (make-store (+ (store-counter store) 1)
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
    (let* ((store (Sigma*-store s*))
           (scps-p (Sigma*-scps-p s*))
           (scps-u (Sigma*-scps-u s*))
           (as (alloc-scope (make-stx 'defs '()) store))
           (scp-defs (car as))
           (s1 (cdr as))
           (ae (alloc-def-env s1))
           (addr-env (car ae))
           (s2 (cdr ae))
           (s3 (def-env-update s2 addr-env env)))
      (cons (make-defs scp-defs addr-env)
            (make-Sigma* s3 (scps-union (list scp-defs) scps-p) scps-u))))

   ;; DEF-BIND (variable: 2 args)
   ((and (pair? ast) (eq? (car ast) 'app)
         (eq? (cadr ast) 'DEF-BIND)
         (= (length ast) 4))
    (let* ((er1 (defs-eval ph (caddr ast) maybe-scp env s*))
           (defs-val (car er1))
           (s*2 (cdr er1))
           (er2 (defs-eval ph (cadddr ast) maybe-scp env s*2))
           (id-arg (car er2))
           (s*3 (cdr er2))
           (store3 (Sigma*-store s*3))
           (scps-p3 (Sigma*-scps-p s*3))
           (scps-u3 (Sigma*-scps-u s*3))
           (scp-defs (defs-scp defs-val))
           (addr-env (defs-addr defs-val))
           (id-flipped (ph-stx-flip ph id-arg maybe-scp))
           (id-pruned (ph-stx-prune ph id-flipped scps-u3))
           (id-defs (ph-stx-add ph id-pruned scp-defs))
           (an (alloc-name id-defs store3))
           (nam-new (car an))
           (s4 (cdr an))
           (s5 (ph-store-bind ph s4 id-defs nam-new))
           (env-defs (def-env-lookup s5 addr-env))
           (s6 (def-env-update s5 addr-env
                               (env-extend env-defs nam-new
                                           (cons 'tvar id-defs)))))
      (cons 0 (make-Sigma* s6 scps-p3 scps-u3))))

   ;; DEF-BIND (macro: 3 args)
   ((and (pair? ast) (eq? (car ast) 'app)
         (eq? (cadr ast) 'DEF-BIND)
         (= (length ast) 5))
    (let* ((er1 (defs-eval ph (caddr ast) maybe-scp env s*))
           (defs-val (car er1))
           (s*2 (cdr er1))
           (er2 (defs-eval ph (cadddr ast) maybe-scp env s*2))
           (id-arg (car er2))
           (s*3 (cdr er2))
           (er3 (defs-eval ph (list-ref ast 4) maybe-scp env s*3))
           (stx-arg (car er3))
           (s*4 (cdr er3))
           (store4 (Sigma*-store s*4))
           (scps-p4 (Sigma*-scps-p s*4))
           (scps-u4 (Sigma*-scps-u s*4))
           (scp-defs (defs-scp defs-val))
           (addr-env (defs-addr defs-val))
           (stx-flipped (ph-stx-flip ph stx-arg maybe-scp))
           (stx-added (ph-stx-add ph stx-flipped scp-defs))
           (s*rhs (make-Sigma* store4 '() '()))
           (exp-er (defs-expand (+ ph 1) stx-added (primitives-env) s*rhs))
           (stx-exp (car exp-er))
           (s5 (Sigma*-store (cdr exp-er)))
           (s*eval (make-Sigma* s5 scps-p4 '()))
           (eval-er (defs-eval ph (ph-parse (+ ph 1) stx-exp s5)
                               #f env s*eval))
           (val-exp (car eval-er))
           (s6 (Sigma*-store (cdr eval-er)))
           (env-defs (def-env-lookup s6 addr-env))
           (id-flipped (ph-stx-flip ph id-arg maybe-scp))
           (id-pruned (ph-stx-prune ph id-flipped scps-u4))
           (id-defs (ph-stx-add ph id-pruned scp-defs))
           (an (alloc-name id-defs s6))
           (nam-new (car an))
           (s7 (cdr an))
           (s8 (ph-store-bind ph s7 id-defs nam-new))
           (s9 (def-env-update s8 addr-env
                               (env-extend env-defs nam-new val-exp))))
      (cons 0 (make-Sigma* s9 scps-p4 scps-u4))))

   ;; LOCAL-EXPAND with definition context (3 args)
   ((and (pair? ast) (eq? (car ast) 'app)
         (eq? (cadr ast) 'LOCAL-EXPAND)
         (= (length ast) 5))
    (let* ((er1 (defs-eval ph (caddr ast) maybe-scp env s*))
           (stx (car er1))
           (s*2 (cdr er1))
           (er2 (defs-eval ph (cadddr ast) maybe-scp env s*2))
           (stop-list (car er2))
           (s*3 (cdr er2))
           (er3 (defs-eval ph (list-ref ast 4) maybe-scp env s*3))
           (defs-val (car er3))
           (s*4 (cdr er3))
           (store4 (Sigma*-store s*4))
           (scp-defs (defs-scp defs-val))
           (addr-env (defs-addr defs-val))
           (env-defs (def-env-lookup store4 addr-env))
           (env-unstops (map (lambda (entry)
                               (cons (car entry) (unstop (cdr entry))))
                             env-defs))
           (stops (if (and (pair? stop-list) (eq? (car stop-list) 'list-val))
                      (cdr stop-list)
                      '()))
           (env-stops
            (env-extend* env-unstops
                         (map (lambda (id-stop)
                                (let ((resolved (ph-resolve ph id-stop store4)))
                                  (cons resolved
                                        (tstop (env-lookup env-defs resolved)))))
                              stops)))
           (stx-flipped (ph-stx-flip ph stx maybe-scp))
           (stx-added (ph-stx-add ph stx-flipped scp-defs))
           (er4 (defs-expand ph stx-added env-stops s*4))
           (stx-exp (car er4))
           (s*5 (cdr er4)))
      (cons (ph-stx-flip ph stx-exp maybe-scp) s*5)))

   ;; BOX
   ((and (pair? ast) (eq? (car ast) 'app)
         (eq? (cadr ast) 'BOX))
    (let* ((er (defs-eval ph (caddr ast) maybe-scp env s*))
           (val (car er))
           (s*2 (cdr er))
           (store2 (Sigma*-store s*2))
           (ab (alloc-box store2))
           (addr (car ab))
           (s3 (cdr ab))
           (s4 (box-update s3 addr val)))
      (cons addr (make-Sigma* s4 (Sigma*-scps-p s*2) (Sigma*-scps-u s*2)))))

   ;; UNBOX
   ((and (pair? ast) (eq? (car ast) 'app)
         (eq? (cadr ast) 'UNBOX))
    (let* ((er (defs-eval ph (caddr ast) maybe-scp env s*))
           (addr (car er))
           (s*2 (cdr er))
           (store2 (Sigma*-store s*2)))
      (cons (box-lookup store2 addr) s*2)))

   ;; SET-BOX!
   ((and (pair? ast) (eq? (car ast) 'app)
         (eq? (cadr ast) 'SET-BOX!))
    (let* ((er1 (defs-eval ph (caddr ast) maybe-scp env s*))
           (addr (car er1))
           (s*2 (cdr er1))
           (er2 (defs-eval ph (cadddr ast) maybe-scp env s*2))
           (val (car er2))
           (s*3 (cdr er2))
           (store3 (Sigma*-store s*3))
           (s4 (box-update store3 addr val)))
      (cons val (make-Sigma* s4 (Sigma*-scps-p s*3) (Sigma*-scps-u s*3)))))

   ;; LOCAL-VALUE
   ((and (pair? ast) (eq? (car ast) 'app)
         (eq? (cadr ast) 'LOCAL-VALUE))
    (let* ((er (defs-eval ph (caddr ast) maybe-scp env s*))
           (id-result (car er))
           (s*2 (cdr er))
           (store2 (Sigma*-store s*2)))
      (cons (env-lookup env (ph-resolve ph id-result store2)) s*2)))

   ;; LOCAL-EXPAND (2 args, no defs)
   ((and (pair? ast) (eq? (car ast) 'app)
         (eq? (cadr ast) 'LOCAL-EXPAND)
         (= (length ast) 4))
    (let* ((er1 (defs-eval ph (caddr ast) maybe-scp env s*))
           (stx (car er1))
           (s*2 (cdr er1))
           (er2 (defs-eval ph (cadddr ast) maybe-scp env s*2))
           (stop-list (car er2))
           (s*3 (cdr er2))
           (store3 (Sigma*-store s*3))
           (env-unstops (map (lambda (entry)
                               (cons (car entry) (unstop (cdr entry))))
                             env))
           (stops (if (and (pair? stop-list) (eq? (car stop-list) 'list-val))
                      (cdr stop-list)
                      '()))
           (env-stops
            (env-extend* env-unstops
                         (map (lambda (id-stop)
                                (let ((resolved (ph-resolve ph id-stop store3)))
                                  (cons resolved
                                        (tstop (env-lookup env resolved)))))
                              stops)))
           (stx-flipped (ph-stx-flip ph stx maybe-scp))
           (er3 (defs-expand ph stx-flipped env-stops s*3))
           (stx-exp (car er3))
           (s*4 (cdr er3)))
      (cons (ph-stx-flip ph stx-exp maybe-scp) s*4)))

   ;; LOCAL-BINDER
   ((and (pair? ast) (eq? (car ast) 'app)
         (eq? (cadr ast) 'LOCAL-BINDER))
    (let* ((er (defs-eval ph (caddr ast) maybe-scp env s*))
           (id-result (car er))
           (s*2 (cdr er))
           (scps-u2 (Sigma*-scps-u s*2)))
      (cons (ph-stx-prune ph id-result scps-u2) s*2)))

   ;; function application
   ((and (pair? ast) (eq? (car ast) 'app)
         (pair? (cadr ast)) (eq? (car (cadr ast)) 'fun))
    (let* ((rator (cadr ast))
           (bvar (cadr (cadr rator)))
           (body (caddr rator))
           (er (defs-eval ph (caddr ast) maybe-scp env s*))
           (val-arg (car er))
           (s*2 (cdr er)))
      (defs-eval ph (subst body bvar val-arg) maybe-scp env s*2)))

   ;; primitive application
   ((and (pair? ast) (eq? (car ast) 'app)
         (prim? (cadr ast)))
    (let* ((er (defs-eval* ph '() (cddr ast) maybe-scp env s*))
           (vals (car er))
           (s*2 (cdr er)))
      (cons (delta (cadr ast) vals) s*2)))

   ;; value
   (else (cons ast s*))))

(define (defs-eval* ph done todo maybe-scp env s*)
  (if (null? todo)
      (cons (reverse done) s*)
      (let ((er (defs-eval ph (car todo) maybe-scp env s*)))
        (defs-eval* ph (cons (car er) done) (cdr todo) maybe-scp env (cdr er)))))

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
          (cons stx s*))

         ;; lambda
         ((and (stx? first)
               (eq? (ph-resolve ph first store) 'lambda))
          (let* ((id-arg (cadr form))
                 (body (caddr form))
                 (an (alloc-name id-arg store))
                 (nam-new (car an))
                 (s1 (cdr an))
                 (as (alloc-scope id-arg s1))
                 (scp-new (car as))
                 (s2 (cdr as))
                 (id-new (ph-stx-add ph id-arg scp-new))
                 (s3 (ph-store-bind ph s2 id-new nam-new))
                 (env-new (env-extend env nam-new (cons 'tvar id-new)))
                 (body-added (ph-stx-add ph body scp-new))
                 (s*inner (make-Sigma* s3
                                       (scps-union (list scp-new) scps-p)
                                       '()))
                 (er (defs-expand ph body-added env-new s*inner)))
            (cons (make-stx (list first id-new (car er)) (stx-ctx stx))
                  (make-Sigma* (Sigma*-store (cdr er)) scps-p scps-u))))

         ;; quote
         ((and (stx? first)
               (eq? (ph-resolve ph first store) 'quote))
          (cons stx s*))

         ;; syntax
         ((and (stx? first)
               (eq? (ph-resolve ph first store) 'syntax))
          (let ((pruned (ph-stx-prune ph (cadr form) scps-p)))
            (cons (make-stx (list first pruned) (stx-ctx stx)) s*)))

         ;; let-syntax
         ((and (stx? first)
               (eq? (ph-resolve ph first store) 'let-syntax))
          (let* ((id (cadr form))
                 (rhs (caddr form))
                 (body (cadddr form))
                 (an (alloc-name id store))
                 (nam-new (car an))
                 (s1 (cdr an))
                 (as (alloc-scope id s1))
                 (scp-new (car as))
                 (s2 (cdr as))
                 (id-new (ph-stx-add ph id scp-new))
                 (s3 (ph-store-bind ph s2 id-new nam-new))
                 (s*rhs (make-Sigma* s3 '() '()))
                 (rhs-er (defs-expand (+ ph 1) rhs (primitives-env) s*rhs))
                 (stx-exp (car rhs-er))
                 (s4 (Sigma*-store (cdr rhs-er)))
                 (s*eval (make-Sigma* s4 scps-p '()))
                 (eval-er (defs-eval ph (ph-parse (+ ph 1) stx-exp s4)
                                     #f env s*eval))
                 (val-exp (car eval-er))
                 (s5 (Sigma*-store (cdr eval-er)))
                 (env-new (env-extend env nam-new val-exp))
                 (body-added (ph-stx-add ph body scp-new))
                 (s*body (make-Sigma* s5
                                      (scps-union (list scp-new) scps-p)
                                      '())))
            (let ((er (defs-expand ph body-added env-new s*body)))
              (cons (car er)
                    (make-Sigma* (Sigma*-store (cdr er)) scps-p scps-u)))))

;; macro invocation
          ((and (stx? first)
               (let ((t (env-lookup env (ph-resolve ph first store))))
                 (and (not (eq? t (ph-resolve ph first store)))
                      (not (and (pair? t) (eq? (car t) 'tvar))))))
          (let* ((as1 (alloc-scope (make-stx 'u '()) store))
                 (scp-u (car as1))
                 (s1 (cdr as1))
                 (as2 (alloc-scope (make-stx 'i '()) s1))
                 (scp-i (car as2))
                 (s2 (cdr as2))
                 (val (env-lookup env (ph-resolve ph first store)))
                 (s*3 (make-Sigma* s2
                                   (scps-union (list scp-u) scps-p)
                                   (scps-union (list scp-u) scps-u)))
                 (stx-added (ph-stx-add ph stx scp-u))
                 (stx-flipped (ph-stx-flip ph stx-added scp-i))
                 (eval-er (defs-eval ph `(app ,val ,stx-flipped)
                                     scp-i env s*3))
                 (stx-exp (car eval-er))
                 (s*4 (cdr eval-er))
                 (result-flipped (ph-stx-flip ph stx-exp scp-i)))
            (defs-expand ph result-flipped env s*4)))

         ;; application
         (else
          (let* ((s*app (make-Sigma* store scps-p '()))
                 (er (defs-expand* ph '() form env s*app)))
            (cons (make-stx (car er) (stx-ctx stx))
                  (make-Sigma* (cadr er) scps-p scps-u)))))))

     ;; identifier
     ((stx? stx)
      (let ((transform (env-lookup env (ph-resolve ph stx store))))
        (if (and (pair? transform) (eq? (car transform) 'tvar))
            (cons (cdr transform) s*)
            (cons stx s*))))
     (else (cons stx s*)))))

(define (defs-expand* ph done todo env s*)
  (if (null? todo)
      (list (reverse done) (Sigma*-store s*))
      (let* ((s*cur (make-Sigma* (Sigma*-store s*) (Sigma*-scps-p s*) '()))
             (er (defs-expand ph (car todo) env s*cur)))
        (defs-expand* ph (cons (car er) done) (cdr todo) env
                      (make-Sigma* (Sigma*-store (cdr er))
                                   (Sigma*-scps-p s*)
                                   '())))))

;;; ----------------------------------------
;;; Helpers

(define (defs-as-syntax datum)
  (cond
   ((pair? datum)
    (make-stx (map defs-as-syntax datum) '()))
   (else
    (make-stx datum '()))))
