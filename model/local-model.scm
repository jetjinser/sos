;;; local-model.scm
;;; Guile implementation of the local-expansion model
;;; (1:1 translation from local-model.rkt, PLT Redex)
;;; Extends phases-model with Σ*, TStop, LOCAL-VALUE/EXPAND/BINDER.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (local-model)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 receive)
  #:use-module (core-model)
  #:use-module (phases-model)
  #:use-module (ssv emit)
  #:export (loc-eval loc-expand ph-gen-temps))

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
    (let*-values ([(id-result s*2) (loc-eval ph (caddr ast) maybe-scp env s*)]
                  [(store2)        (Sigma*-store s*2)]
                  [(result)        (env-lookup env (ph-resolve ph id-result store2))])
      (emit-rule 'LOCAL-VALUE ast result store2 env (list (cons 'phase ph)))
      (values result s*2)))

   ;; LOCAL-EXPAND
   ((and (pair? ast) (eq? (car ast) 'app)
         (eq? (cadr ast) 'LOCAL-EXPAND))
    (let*-values ([(stx s*2)       (loc-eval ph (caddr ast) maybe-scp env s*)]
                  [(stop-list s*3) (loc-eval ph (cadddr ast) maybe-scp env s*2)]
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
                  [(stx-exp s*4)   (loc-expand ph stx-flipped env-stops s*3)]
                  [(result)        (ph-stx-flip ph stx-exp maybe-scp)])
      (emit-rule 'LOCAL-EXPAND ast result (Sigma*-store s*4) env (list (cons 'phase ph)))
      (values result s*4)))

   ;; LOCAL-BINDER
   ((and (pair? ast) (eq? (car ast) 'app)
         (eq? (cadr ast) 'LOCAL-BINDER))
    (let*-values ([(id-result s*2) (loc-eval ph (caddr ast) maybe-scp env s*)]
                  [(scps-u2)       (Sigma*-scps-u s*2)]
                  [(result)        (ph-stx-prune ph id-result scps-u2)])
      (emit-rule 'LOCAL-BINDER ast result (Sigma*-store s*2) env (list (cons 'phase ph)))
      (values result s*2)))

   ;; function application
   ((and (pair? ast) (eq? (car ast) 'app)
         (pair? (cadr ast)) (eq? (car (cadr ast)) 'fun))
    (let*-values ([(rator)          (cadr ast)]
                  [(bvar)           (cadr (cadr rator))]
                  [(body)           (caddr rator)]
                  [(val-arg s*2)    (loc-eval ph (caddr ast) maybe-scp env s*)]
                  [(result s*3)     (loc-eval ph (subst body bvar val-arg) maybe-scp env s*2)])
      (emit-rule 'fun-app ast result (Sigma*-store s*3) env (list (cons 'phase ph)))
      (values result s*3)))

   ;; free-identifier=?
   ((and (pair? ast) (eq? (car ast) 'app)
         (eq? (cadr ast) 'free-identifier=?))
    (let*-values ([(va s*2) (loc-eval ph (caddr ast) maybe-scp env s*)]
                  [(vb s*3) (loc-eval ph (cadddr ast) maybe-scp env s*2)]
                  [(store3) (Sigma*-store s*3)])
      (values (eq? (ph-resolve ph va store3) (ph-resolve ph vb store3)) s*3)))

    ;; generate-temporaries
    ((and (pair? ast) (eq? (car ast) 'app)
          (eq? (cadr ast) 'generate-temporaries))
     (let*-values ([(v s*2)     (loc-eval ph (caddr ast) maybe-scp env s*)]
                   [(temps s*3) (ph-gen-temps ph v s*2)])
       (values temps s*3)))

    ;; if
    ((and (pair? ast) (eq? (car ast) 'app)
          (eq? (cadr ast) 'if))
     (let*-values ([(cv s*2) (loc-eval ph (caddr ast) maybe-scp env s*)])
       (if cv
           (loc-eval ph (cadddr ast) maybe-scp env s*2)
           (loc-eval ph (list-ref ast 4) maybe-scp env s*2))))

    ;; syntax template
    ((and (pair? ast) (eq? (car ast) 'tmpl))
     (let ((result (eval-tmpl (cadr ast))))
       (emit-rule 'tmpl ast result (Sigma*-store s*) env (list (cons 'phase ph)))
       (values result s*)))

    ;; syntax-case
    ((and (pair? ast) (eq? (car ast) 'scase))
     (let*-values ([(s s*2) (loc-eval ph (cadr ast) maybe-scp env s*)]
                   [(result s*3)
                    (scase-match s (caddr ast) (cdddr ast) (Sigma*-store s*2)
                                 (lambda (a store)
                                   (loc-eval ph a maybe-scp env
                                             (make-Sigma* store
                                                          (Sigma*-scps-p s*2)
                                                          (Sigma*-scps-u s*2))))
                                 (lambda (id) (ph-resolve ph id (Sigma*-store s*2))))])
       (emit-rule 'scase ast result (Sigma*-store s*3) env (list (cons 'phase ph)))
       (values result s*3)))

    ;; foreign (host-procedure) transformer
   ((and (pair? ast) (eq? (car ast) 'app)
         (procedure? (cadr ast)))
    (let*-values ([(vals s*2) (loc-eval* ph '() (cddr ast) maybe-scp env s*)])
      (values (apply (cadr ast) vals) s*2)))

   ;; primitive application
   ((and (pair? ast) (eq? (car ast) 'app)
         (prim? (cadr ast)))
    (let*-values ([(vals s*2) (loc-eval* ph '() (cddr ast) maybe-scp env s*)]
                  [(result)   (delta (cadr ast) vals)])
      (emit-rule 'prim-app ast result (Sigma*-store s*2) env (list (cons 'phase ph)))
      (values result s*2)))

   ;; value
   (else
    (emit-rule 'value ast ast (Sigma*-store s*) env (list (cons 'phase ph)))
    (values ast s*))))

(define (loc-eval* ph done todo maybe-scp env s*)
  (if (null? todo)
      (values (reverse done) s*)
      (receive (val s*2) (loc-eval ph (car todo) maybe-scp env s*)
        (loc-eval* ph (cons val done) (cdr todo) maybe-scp env s*2))))

(define (ph-gen-temps ph stxs s*)
  (if (null? stxs)
      (values '() s*)
      (let*-values ([(scp s1)    (alloc-scope (car stxs) (Sigma*-store s*))]
                    [(freshened) (ph-stx-add ph (car stxs) scp)]
                    [(s*2)       (make-Sigma* s1 (Sigma*-scps-p s*) (Sigma*-scps-u s*))]
                    [(rest s*3)  (ph-gen-temps ph (cdr stxs) s*2)])
        (values (cons freshened rest) s*3))))

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
          (values stx s*))

         ;; lambda
         ((and (stx? first)
               (eq? (ph-resolve ph first store) 'lambda))
          (let*-values ([(id-arg)      (cadr form)]
                        [(body)        (caddr form)]
                        [(nam-new s1)  (alloc-name id-arg store)]
                        [(scp-new s2)  (alloc-scope id-arg s1)]
                        [(id-new)      (ph-stx-add ph id-arg scp-new)]
                        [(s3)          (ph-store-bind ph s2 id-new nam-new)]
                        [(env-new)     (env-extend env nam-new (cons 'tvar id-new))]
                        [(body-added)  (ph-stx-add ph body scp-new)]
                        [(s*inner)     (make-Sigma* s3
                                                    (scps-union (list scp-new) scps-p)
                                                    '())]
                        [(body-exp s*4) (loc-expand ph body-added env-new s*inner)])
            (let ([result (make-stx (list first id-new body-exp) (stx-ctx stx) (stx-span stx))]
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
                 (result (make-stx (list first pruned) (stx-ctx stx) (stx-span stx))))
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
                         [(stx-exp s*4) (loc-expand (+ ph 1) rhs (for-syntax-env) s*rhs)]
                        [(s4)          (Sigma*-store s*4)]
                        [(s*eval)      (make-Sigma* s4 scps-p '())]
                        [(val-exp s*5) (loc-eval ph (ph-parse (+ ph 1) stx-exp s4)
                                                 #f env s*eval)]
                        [(s5)          (Sigma*-store s*5)]
                        [(env-new)     (env-extend env nam-new val-exp)]
                        [(body-added)  (ph-stx-add ph body scp-new)]
                        [(s*body)      (make-Sigma* s5
                                                    (scps-union (list scp-new) scps-p)
                                                    '())]
                        [(body-exp s*6) (loc-expand ph body-added env-new s*body)])
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
           (let*-values ([(scp-u s1)    (alloc-scope (make-stx 'a '() #f) store)]
                         [(scp-i s2)    (alloc-scope (make-stx 'a '() #f) s1)]
                        [(val)         (env-lookup env (ph-resolve ph first store))]
                        [(s*3)         (make-Sigma* s2
                                                    (scps-union (list scp-u) scps-p)
                                                    (scps-union (list scp-u) scps-u))]
                        [(stx-added)   (ph-stx-add ph stx scp-u)]
                        [(stx-flipped) (ph-stx-flip ph stx-added scp-i)]
                        [(stx-exp s*4) (loc-eval ph `(app ,val ,stx-flipped)
                                                 scp-i env s*3)]
                        [(result-flipped) (ph-stx-flip ph stx-exp scp-i)]
                        [(expanded s*5)   (loc-expand ph result-flipped env s*4)])
            (emit-rule 'macro-invoke stx expanded (Sigma*-store s*5) env
                       (list (cons 'phase ph) (cons 'scp-u scp-u) (cons 'scp-i scp-i)))
            (values expanded s*5)))

         ;; application
         (else
          (let*-values ([(s*app)            (make-Sigma* store scps-p '())]
                        [(expanded store-out) (loc-expand* ph '() form env s*app)]
                        [(result)           (make-stx expanded (stx-ctx stx) (stx-span stx))]
                        [(s*out)            (make-Sigma* store-out scps-p scps-u)])
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

(define (loc-expand* ph done todo env s*)
  (if (null? todo)
      (values (reverse done) (Sigma*-store s*))
      (let*-values ([(s*cur)          (make-Sigma* (Sigma*-store s*) (Sigma*-scps-p s*) '())]
                    [(expanded s*out) (loc-expand ph (car todo) env s*cur)])
        (loc-expand* ph (cons expanded done) (cdr todo) env
                     (make-Sigma* (Sigma*-store s*out)
                                  (Sigma*-scps-p s*)
                                  '())))))

