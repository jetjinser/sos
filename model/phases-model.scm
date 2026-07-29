;;; phases-model.scm
;;; Guile implementation of the multi-phase scope-set model
;;; (1:1 translation from phases-model.rkt, PLT Redex)
;;; Extends core-model with phase-indexed contexts.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (phases-model)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 receive)
  #:use-module (core-model)
  #:use-module (ssv emit)
  #:export (ph-stx-add ph-stx-flip ph-stx-prune
            update-ctx at-phase
            ph-store-bind ph-resolve
            ph-parse
            ph-expand ph-expand*))

;;; ----------------------------------------
;;; Context operations (phase-indexed map)
;;; ctx = ((ph . scps) ...)

(define (at-phase ctx ph)
  (let ((entry (assq ph ctx)))
    (if entry (cdr entry) '())))

(define (update-ctx ctx ph scps)
  (let ((entry (assq ph ctx)))
    (if entry
        (map (lambda (e)
               (if (eq? (car e) ph)
                   (cons ph scps)
                   e))
             ctx)
        (append ctx (list (cons ph scps))))))

;;; ----------------------------------------
;;; Syntax object operations (phase-aware)

(define (%ph-stx-add ph stx scp)
  (let ((form (stx-form stx)))
    (make-stx (if (pair? form)
                  (map (lambda (s) (%ph-stx-add ph s scp)) form)
                  form)
              (update-ctx (stx-ctx stx) ph
                          (scps-union (list scp) (at-phase (stx-ctx stx) ph)))
              (stx-span stx))))

(define (ph-stx-add ph stx scp)
  (let ((r (%ph-stx-add ph stx scp)))
    (emit-op 'stx-add scp stx r)
    r))

(define (%ph-stx-flip ph stx scp)
  (let ((form (stx-form stx)))
    (make-stx (if (pair? form)
                  (map (lambda (s) (%ph-stx-flip ph s scp)) form)
                  form)
              (update-ctx (stx-ctx stx) ph
                          (scps-addremove scp (at-phase (stx-ctx stx) ph)))
              (stx-span stx))))

(define (ph-stx-flip ph stx scp)
  (let ((r (%ph-stx-flip ph stx scp)))
    (emit-op 'stx-flip scp stx r)
    r))

(define (%ph-stx-prune ph stx scps-p)
  (let ((form (stx-form stx)))
    (make-stx (if (pair? form)
                  (map (lambda (s) (%ph-stx-prune ph s scps-p)) form)
                  form)
              (update-ctx (stx-ctx stx) ph
                          (scps-subtract (at-phase (stx-ctx stx) ph) scps-p))
              (stx-span stx))))

(define (ph-stx-prune ph stx scps-p)
  (let ((r (%ph-stx-prune ph stx scps-p)))
    (emit-op 'stx-prune scps-p stx r)
    r))

;;; ----------------------------------------
;;; Store operations (phase-aware)

(define (ph-store-bind ph store id name)
  (let* ((sym (stx-form id))
         (scopes (at-phase (stx-ctx id) ph))
         (binds (store-binds store))
         (existing (assq sym binds)))
    (emit-op 'bind sym scopes name)
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

(define (ph-resolve ph id store)
  (let* ((sym (stx-form id))
         (ctx (at-phase (stx-ctx id) ph))
         (bindings (store-lookup store sym)))
    (if (null? bindings)
        sym
        (let ((biggest (biggest-subset ctx (map car bindings))))
          (if biggest
              (or (binding-lookup bindings biggest) sym)
              sym)))))

;;; ----------------------------------------
;;; Parse (phase-aware)

(define (ph-parse ph stx store)
  (let ((form (stx-form stx)))
    (cond
     ((pair? form)
      (let ((first (car form)))
        (cond
         ((and (stx? first)
               (eq? (ph-resolve ph first store) 'lambda))
          (let ((id-arg (cadr form))
                (body (caddr form)))
            `(fun (var ,(ph-resolve ph id-arg store))
                  ,(ph-parse ph body store))))
         ((and (stx? first)
               (eq? (ph-resolve ph first store) 'quote))
          (stx-strip (cadr form)))
         ((and (stx? first)
               (eq? (ph-resolve ph first store) 'syntax))
          (cadr form))
         (else
          (cons 'app (map (lambda (s) (ph-parse ph s store)) form))))))
     ((or (number? form) (prim? form)) form)
     (else `(var ,(ph-resolve ph stx store))))))

;;; ----------------------------------------
;;; Expand (phase-aware)

(define (ph-expand ph stx env scps-p store)
  (let ((form (stx-form stx)))
    (cond
     ((pair? form)
      (let ((first (car form)))
        (cond
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
                        [(body-exp s4) (ph-expand ph body-added env-new
                                                  (scps-union (list scp-new) scps-p) s3)])
            (let ([result (make-stx (list first id-new body-exp) (stx-ctx stx) (stx-span stx))])
              (emit-rule 'lambda stx result s4 env
                         (list (cons 'phase ph) (cons 'name nam-new) (cons 'scope scp-new)))
              (values result s4))))
         ;; quote
         ((and (stx? first)
               (eq? (ph-resolve ph first store) 'quote))
          (emit-rule 'quote stx stx store env (list (cons 'phase ph)))
          (values stx store))
         ;; syntax
         ((and (stx? first)
               (eq? (ph-resolve ph first store) 'syntax))
          (let* ((pruned (ph-stx-prune ph (cadr form) scps-p))
                  (result (make-stx (list first pruned) (stx-ctx stx) (stx-span stx))))
            (emit-rule 'syntax stx result store env (list (cons 'phase ph)))
            (values result store)))
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
                        [(stx-exp s4)  (ph-expand (+ ph 1) rhs (primitives-env) '() s3)]
                        [(transformer) (eval-ast (ph-parse (+ ph 1) stx-exp s4))]
                        [(env-new)     (env-extend env nam-new transformer)]
                        [(body-added)  (ph-stx-add ph body scp-new)]
                        [(scps-p2)     (scps-union (list scp-new) scps-p)]
                        [(body-exp s5) (ph-expand ph body-added env-new scps-p2 s4)])
            (emit-rule 'let-syntax stx body-exp s5 env
                       (list (cons 'phase ph) (cons 'name nam-new) (cons 'scope scp-new)))
            (values body-exp s5)))
         ;; macro invocation
         ((and (stx? first)
               (not (eq? (env-lookup env (ph-resolve ph first store))
                         (ph-resolve ph first store))))
           (let*-values ([(scp-u s1)       (alloc-scope (make-stx 'a '() #f) store)]
                         [(scp-i s2)       (alloc-scope (make-stx 'a '() #f) s1)]
                        [(val)            (env-lookup env (ph-resolve ph first store))]
                        [(stx-added)      (ph-stx-add ph stx scp-u)]
                        [(stx-flipped)    (ph-stx-flip ph stx-added scp-i)]
                        [(result)         (eval-ast `(app ,val ,stx-flipped))]
                        [(result-flipped) (ph-stx-flip ph result scp-i)]
                        [(expanded s3)    (ph-expand ph result-flipped env
                                                     (scps-union (list scp-u) scps-p) s2)])
            (emit-rule 'macro-invoke stx expanded s3 env
                       (list (cons 'phase ph) (cons 'scp-u scp-u) (cons 'scp-i scp-i)))
            (values expanded s3)))
         ;; application
         (else
          (let*-values ([(expanded s1) (ph-expand* ph '() form env scps-p store)]
                         [(result)      (make-stx expanded (stx-ctx stx) (stx-span stx))])
            (emit-rule 'app stx result s1 env (list (cons 'phase ph)))
            (values result s1))))))
     ;; identifier
     ((stx? stx)
      (let ((transform (env-lookup env (ph-resolve ph stx store))))
        (if (and (pair? transform) (eq? (car transform) 'tvar))
            (begin
              (emit-rule 'id stx (cdr transform) store env
                         (list (cons 'phase ph) (cons 'tvar (cdr transform))))
              (values (cdr transform) store))
            (begin
              (emit-rule 'id stx stx store env (list (cons 'phase ph)))
              (values stx store)))))
     (else (values stx store)))))

(define (ph-expand* ph done todo env scps-p store)
  (if (null? todo)
      (values (reverse done) store)
      (receive (expanded s1) (ph-expand ph (car todo) env scps-p store)
        (ph-expand* ph (cons expanded done) (cdr todo) env scps-p s1))))

