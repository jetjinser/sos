;;; phases-model.scm
;;; Guile implementation of the multi-phase scope-set model
;;; (1:1 translation from phases-model.rkt, PLT Redex)
;;; Extends core-model with phase-indexed contexts.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (phases-model)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (core-model)
  #:export (ph-stx-add ph-stx-flip ph-stx-prune
            update-ctx at-phase
            ph-store-bind ph-resolve
            ph-parse
            ph-expand ph-expand*
            ph-as-syntax))

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

(define (ph-stx-add ph stx scp)
  (let ((form (stx-form stx)))
    (make-stx (if (pair? form)
                  (map (lambda (s) (ph-stx-add ph s scp)) form)
                  form)
              (update-ctx (stx-ctx stx) ph
                          (scps-union (list scp) (at-phase (stx-ctx stx) ph))))))

(define (ph-stx-flip ph stx scp)
  (let ((form (stx-form stx)))
    (make-stx (if (pair? form)
                  (map (lambda (s) (ph-stx-flip ph s scp)) form)
                  form)
              (update-ctx (stx-ctx stx) ph
                          (scps-addremove scp (at-phase (stx-ctx stx) ph))))))

(define (ph-stx-prune ph stx scps-p)
  (let ((form (stx-form stx)))
    (make-stx (if (pair? form)
                  (map (lambda (s) (ph-stx-prune ph s scps-p)) form)
                  form)
              (update-ctx (stx-ctx stx) ph
                          (scps-subtract (at-phase (stx-ctx stx) ph) scps-p)))))

;;; ----------------------------------------
;;; Store operations (phase-aware)

(define (ph-store-bind ph store id name)
  (let* ((sym (stx-form id))
         (scopes (at-phase (stx-ctx id) ph))
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
                 (er (ph-expand ph body-added env-new
                                (scps-union (list scp-new) scps-p) s3)))
            (cons (make-stx (list first id-new (car er)) (stx-ctx stx))
                  (cdr er))))
         ;; quote
         ((and (stx? first)
               (eq? (ph-resolve ph first store) 'quote))
          (cons stx store))
         ;; syntax
         ((and (stx? first)
               (eq? (ph-resolve ph first store) 'syntax))
          (let ((pruned (ph-stx-prune ph (cadr form) scps-p)))
            (cons (make-stx (list first pruned) (stx-ctx stx)) store)))
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
                 (rhs-er (ph-expand (+ ph 1) rhs (primitives-env) '() s3))
                 (stx-exp (car rhs-er))
                 (s4 (cdr rhs-er))
                 (transformer (eval-ast (ph-parse (+ ph 1) stx-exp s4)))
                 (env-new (env-extend env nam-new transformer))
                 (body-added (ph-stx-add ph body scp-new))
                 (scps-p2 (scps-union (list scp-new) scps-p)))
            (ph-expand ph body-added env-new scps-p2 s4)))
         ;; macro invocation
         ((and (stx? first)
               (not (eq? (env-lookup env (ph-resolve ph first store))
                         (ph-resolve ph first store))))
          (let* ((as1 (alloc-scope (make-stx 'a '()) store))
                 (scp-u (car as1))
                 (s1 (cdr as1))
                 (as2 (alloc-scope (make-stx 'a '()) s1))
                 (scp-i (car as2))
                 (s2 (cdr as2))
                 (val (env-lookup env (ph-resolve ph first store)))
                 (stx-added (ph-stx-add ph stx scp-u))
                 (stx-flipped (ph-stx-flip ph stx-added scp-i))
                 (result (eval-ast `(app ,val ,stx-flipped)))
                 (result-flipped (ph-stx-flip ph result scp-i)))
            (ph-expand ph result-flipped env
                       (scps-union (list scp-u) scps-p) s2)))
         ;; application
         (else
          (let ((er (ph-expand* ph '() form env scps-p store)))
            (cons (make-stx (car er) (stx-ctx stx)) (cdr er)))))))
     ;; identifier
     ((stx? stx)
      (let ((transform (env-lookup env (ph-resolve ph stx store))))
        (if (and (pair? transform) (eq? (car transform) 'tvar))
            (cons (cdr transform) store)
            (cons stx store))))
     (else (cons stx store)))))

(define (ph-expand* ph done todo env scps-p store)
  (if (null? todo)
      (cons (reverse done) store)
      (let ((er (ph-expand ph (car todo) env scps-p store)))
        (ph-expand* ph (cons (car er) done) (cdr todo) env scps-p (cdr er)))))

;;; ----------------------------------------
;;; Helpers

(define (ph-as-syntax datum)
  (cond
   ((pair? datum)
    (make-stx (map ph-as-syntax datum) '()))
   (else
    (make-stx datum '()))))
