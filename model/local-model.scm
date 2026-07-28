;;; local-model.scm
;;; Guile implementation of the local-expansion model
;;; (1:1 translation from local-model.rkt, PLT Redex)
;;; Extends phases-model with Σ*, TStop, LOCAL-VALUE/EXPAND/BINDER.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (local-model)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (core-model)
  #:use-module (phases-model)
  #:use-module (ssv emit)
  #:export (loc-eval loc-expand))

;;; ----------------------------------------
;;; Σ* triple: (store scps-p scps-u)

(define (make-Sigma* store scps-p scps-u)
  (list store scps-p scps-u))

(define (Sigma*-store s*) (car s*))
(define (Sigma*-scps-p s*) (cadr s*))
(define (Sigma*-scps-u s*) (caddr s*))

;;; ----------------------------------------
;;; TStop

(define (tstop transform) (cons 'tstop transform))
(define (tstop? x) (and (pair? x) (eq? (car x) 'tstop)))
(define (tstop-transform x) (cdr x))

(define (unstop transform)
  (if (tstop? transform)
      (tstop-transform transform)
      transform))

;;; ----------------------------------------
;;; Environment helpers

(define (env-extend* env entries)
  (append entries env))

;;; ----------------------------------------
;;; Eval (phase-aware, store-threading)

(define (loc-eval ph ast maybe-scp env s*)
  (cond
   ;; LOCAL-VALUE
   ((and (pair? ast) (eq? (car ast) 'app)
         (eq? (cadr ast) 'LOCAL-VALUE))
    (let* ((er (loc-eval ph (caddr ast) maybe-scp env s*))
           (id-result (car er))
           (s*2 (cdr er))
           (store2 (Sigma*-store s*2))
           (result (env-lookup env (ph-resolve ph id-result store2))))
      (emit-rule 'LOCAL-VALUE ast result store2 env (list (cons 'phase ph)))
      (cons result s*2)))

   ;; LOCAL-EXPAND
   ((and (pair? ast) (eq? (car ast) 'app)
         (eq? (cadr ast) 'LOCAL-EXPAND))
    (let* ((er1 (loc-eval ph (caddr ast) maybe-scp env s*))
           (stx (car er1))
           (s*2 (cdr er1))
           (er2 (loc-eval ph (cadddr ast) maybe-scp env s*2))
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
           (er3 (loc-expand ph stx-flipped env-stops s*3))
           (stx-exp (car er3))
           (s*4 (cdr er3))
           (result (ph-stx-flip ph stx-exp maybe-scp)))
      (emit-rule 'LOCAL-EXPAND ast result (Sigma*-store s*4) env (list (cons 'phase ph)))
      (cons result s*4)))

   ;; LOCAL-BINDER
   ((and (pair? ast) (eq? (car ast) 'app)
         (eq? (cadr ast) 'LOCAL-BINDER))
    (let* ((er (loc-eval ph (caddr ast) maybe-scp env s*))
           (id-result (car er))
           (s*2 (cdr er))
           (scps-u2 (Sigma*-scps-u s*2))
           (result (ph-stx-prune ph id-result scps-u2)))
      (emit-rule 'LOCAL-BINDER ast result (Sigma*-store s*2) env (list (cons 'phase ph)))
      (cons result s*2)))

   ;; function application
   ((and (pair? ast) (eq? (car ast) 'app)
         (pair? (cadr ast)) (eq? (car (cadr ast)) 'fun))
    (let* ((rator (cadr ast))
           (bvar (cadr (cadr rator)))
           (body (caddr rator))
           (er (loc-eval ph (caddr ast) maybe-scp env s*))
           (val-arg (car er))
           (s*2 (cdr er))
           (result-er (loc-eval ph (subst body bvar val-arg) maybe-scp env s*2)))
      (emit-rule 'fun-app ast (car result-er) (Sigma*-store (cdr result-er)) env (list (cons 'phase ph)))
      result-er))

   ;; primitive application
   ((and (pair? ast) (eq? (car ast) 'app)
         (prim? (cadr ast)))
    (let* ((er (loc-eval* ph '() (cddr ast) maybe-scp env s*))
           (vals (car er))
           (s*2 (cdr er))
           (result (delta (cadr ast) vals)))
      (emit-rule 'prim-app ast result (Sigma*-store s*2) env (list (cons 'phase ph)))
      (cons result s*2)))

   ;; value
   (else
    (emit-rule 'value ast ast (Sigma*-store s*) env (list (cons 'phase ph)))
    (cons ast s*))))

(define (loc-eval* ph done todo maybe-scp env s*)
  (if (null? todo)
      (cons (reverse done) s*)
      (let ((er (loc-eval ph (car todo) maybe-scp env s*)))
        (loc-eval* ph (cons (car er) done) (cdr todo) maybe-scp env (cdr er)))))

;;; ----------------------------------------
;;; Expand (with Σ*)

(define (loc-expand ph stx env s*)
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
                 (er (loc-expand ph body-added env-new s*inner))
                 (result (make-stx (list first id-new (car er)) (stx-ctx stx)))
                 (s*out (make-Sigma* (Sigma*-store (cdr er)) scps-p scps-u)))
            (emit-rule 'lambda stx result (Sigma*-store (cdr er)) env
                       (list (cons 'phase ph) (cons 'name nam-new) (cons 'scope scp-new)))
            (cons result s*out)))

         ;; quote
         ((and (stx? first)
               (eq? (ph-resolve ph first store) 'quote))
          (emit-rule 'quote stx stx store env (list (cons 'phase ph)))
          (cons stx s*))

         ;; syntax
         ((and (stx? first)
               (eq? (ph-resolve ph first store) 'syntax))
          (let* ((pruned (ph-stx-prune ph (cadr form) scps-p))
                 (result (make-stx (list first pruned) (stx-ctx stx))))
            (emit-rule 'syntax stx result store env (list (cons 'phase ph)))
            (cons result s*)))

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
                 (rhs-er (loc-expand (+ ph 1) rhs (primitives-env) s*rhs))
                 (stx-exp (car rhs-er))
                 (s4 (Sigma*-store (cdr rhs-er)))
                 (s*eval (make-Sigma* s4 scps-p '()))
                 (eval-er (loc-eval ph (ph-parse (+ ph 1) stx-exp s4)
                                    #f env s*eval))
                 (val-exp (car eval-er))
                 (s5 (Sigma*-store (cdr eval-er)))
                 (env-new (env-extend env nam-new val-exp))
                 (body-added (ph-stx-add ph body scp-new))
                 (s*body (make-Sigma* s5
                                      (scps-union (list scp-new) scps-p)
                                      '())))
            (let ((er (loc-expand ph body-added env-new s*body)))
              (let ((result (car er))
                    (s*out (make-Sigma* (Sigma*-store (cdr er)) scps-p scps-u)))
                (emit-rule 'let-syntax stx result (Sigma*-store (cdr er)) env
                           (list (cons 'phase ph) (cons 'name nam-new) (cons 'scope scp-new)))
                (cons result s*out)))))

;; macro invocation
          ((and (stx? first)
                (let ((t (env-lookup env (ph-resolve ph first store))))
                  (and (not (eq? t (ph-resolve ph first store)))
                       (not (and (pair? t) (eq? (car t) 'tvar))))))
          (let* ((as1 (alloc-scope (make-stx 'a '()) store))
                 (scp-u (car as1))
                 (s1 (cdr as1))
                 (as2 (alloc-scope (make-stx 'a '()) s1))
                 (scp-i (car as2))
                 (s2 (cdr as2))
                 (val (env-lookup env (ph-resolve ph first store)))
                 (s*3 (make-Sigma* s2
                                   (scps-union (list scp-u) scps-p)
                                   (scps-union (list scp-u) scps-u)))
                 (stx-added (ph-stx-add ph stx scp-u))
                 (stx-flipped (ph-stx-flip ph stx-added scp-i))
                 (eval-er (loc-eval ph `(app ,val ,stx-flipped)
                                    scp-i env s*3))
                 (stx-exp (car eval-er))
                 (s*4 (cdr eval-er))
                 (result-flipped (ph-stx-flip ph stx-exp scp-i))
                 (er (loc-expand ph result-flipped env s*4)))
            (emit-rule 'macro-invoke stx (car er) (Sigma*-store (cdr er)) env
                       (list (cons 'phase ph) (cons 'scp-u scp-u) (cons 'scp-i scp-i)))
            er))

         ;; application
         (else
          (let* ((s*app (make-Sigma* store scps-p '()))
                 (er (loc-expand* ph '() form env s*app))
                 (result (make-stx (car er) (stx-ctx stx)))
                 (s*out (make-Sigma* (cadr er) scps-p scps-u)))
            (emit-rule 'app stx result (cadr er) env (list (cons 'phase ph)))
            (cons result s*out))))))

     ;; identifier
     ((stx? stx)
      (let ((transform (env-lookup env (ph-resolve ph stx store))))
        (if (and (pair? transform) (eq? (car transform) 'tvar))
            (begin
              (emit-rule 'id stx (cdr transform) store env
                         (list (cons 'phase ph) (cons 'tvar (cdr transform))))
              (cons (cdr transform) s*))
            (begin
              (emit-rule 'id stx stx store env (list (cons 'phase ph)))
              (cons stx s*)))))
     (else
      (emit-rule 'literal stx stx store env (list (cons 'phase ph)))
      (cons stx s*)))))

(define (loc-expand* ph done todo env s*)
  (if (null? todo)
      (list (reverse done) (Sigma*-store s*))
      (let* ((s*cur (make-Sigma* (Sigma*-store s*) (Sigma*-scps-p s*) '()))
             (er (loc-expand ph (car todo) env s*cur)))
        (loc-expand* ph (cons (car er) done) (cdr todo) env
                     (make-Sigma* (Sigma*-store (cdr er))
                                  (Sigma*-scps-p s*)
                                  '())))))

;;; ----------------------------------------
;;; Helpers

(define (loc-as-syntax datum)
  (cond
   ((pair? datum)
    (make-stx (map loc-as-syntax datum) '()))
   (else
    (make-stx datum '()))))
