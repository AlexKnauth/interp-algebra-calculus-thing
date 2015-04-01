#lang racket/base

(provide (all-defined-out))

(require racket/match
         racket/contract/base
         racket/list
         unstable/list
         generic-bind/as-rkt-names
         rackjure/conditionals
         my-plai/my-type-case
         kw-utils/keyword-lambda
         kw-utils/keyword-apply-sort
         "stuff.rkt"
         )
(module+ test
  (require rackunit
           ))

(define-type ExprC
  [valC [val any/c]]
  [idC [sym symbol?]]
  [letC [vars (listof (list/c symbol? ExprC?))] [body ExprC?]]
  [fnC [syms (listof symbol?)] [body ExprC?]]
  [appC [f ExprC?] [args (listof (or/c ExprC? (list/c keyword? ExprC?)))]]
  [ifC [a ExprC?] [b ExprC?] [c ExprC?]])
;; Because of the substitute function, identifiers in the sense of idC cannot be mutable.
;; However, there is nothing preventing mutable data structures such as mutable boxes from being used
;; in a valC, and nothing preventing an identifier macro from expanding to a reference to a mutable
;; box, so things that look like variables are still possible in the surface syntax.

(struct syntax-binding (val) #:transparent)

;; an Env is an immutable (Hasheqof Symbol (U (valC Any) (syntax-binding Any))
(define env?
  (flat-named-contract
   'env?
   (ihasheq/c symbol? (or/c valC? syntax-binding?))))

(define/contract empty-env env? (hasheq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; interp-expanded : ExprC Env -> Any
(define/contract (interp-expanded exprC env)
  [ExprC? env? . -> . any/c]
  (my-type-case ExprC exprC
    [(valC val) val]
    [(letC `(,ps ...) body)
     (define orig-env env)
     (when-let [it (check-duplicate (map first ps))]
       (error 'let "duplicate identifier: ~a" it))
     (interp-expanded
      body
      (for/fold ([env orig-env]) ([($ `[,var ,val]) (in-list ps)])
        (hash-set env var (valC (interp-expanded val orig-env)))))]
    [(fnC syms body)
     (fnV syms body env)]
    [(idC (? symbol? sym))
     (match (hash-ref env sym)
       [(valC val) val])]
    [(appC f args)
     (define fv (interp-expanded f env))
     (define-values (kws kw-args rev-rst-args)
       (for/fold ([kws '()] [kw-args '()] [rev-rst-args '()])
                 ([arg (in-list args)])
         (match arg
           [(? ExprC? arg) (values kws
                                   kw-args
                                   (cons (interp-expanded arg env) rev-rst-args))]
           [(list (? keyword? kw) (? ExprC? arg))
            (values (cons kw kws)
                    (cons (interp-expanded arg env) kw-args)
                    rev-rst-args)])))
     (keyword-apply/sort
      fv
      kws
      kw-args
      (reverse rev-rst-args))]
    [(ifC a b c)
     (if (interp-expanded a env)
         (interp-expanded b env)
         (interp-expanded c env))]
    ))

(struct fnV (syms body env) #:transparent
  ;#:guard
  ;(lambda (syms body env _)
  ;  (values new-syms new-body env)
  ;  )
  #:property prop:procedure
  (keyword-lambda (kws kw-args this)
    (match-define (fnV syms body env) this)
    (define given-syms (map keyword->symbol kws))
    (for ([sym (in-list given-syms)])
      (unless (member sym syms)
        (error "bad")))
    (define rst-syms (remove* given-syms syms))
    (cond
      [(empty? rst-syms)
       (define/contract body-env env?
         (for/fold ([env env]) ([sym (in-list given-syms)]
                                [arg (in-list kw-args)])
           (hash-set env sym (valC arg))))
       (interp-expanded body body-env)]
      [(empty? given-syms) this]
      [else
       (fnV
        rst-syms
        body
        (for/fold ([env env]) ([sym (in-list given-syms)]
                               [arg (in-list kw-args)])
          (hash-set env sym (valC arg))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

