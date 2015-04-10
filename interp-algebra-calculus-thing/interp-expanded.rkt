#lang racket/base

(provide (all-defined-out))

(require racket/match
         racket/contract/base
         racket/list
         syntax/srcloc
         syntax/location
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
  [valC [val any/c] [srcloc source-location?]]
  [idC [sym symbol?] [srcloc source-location?]]
  [letC [vars (listof (list/c idC? ExprC?))] [body ExprC?] [srcloc source-location?]]
  [fnC [ids (listof idC?)] [body ExprC?] [srcloc source-location?]]
  [appC [f ExprC?] [args (listof (or/c ExprC? (list/c keyword? ExprC?)))] [srcloc source-location?]]
  [ifC [a ExprC?] [b ExprC?] [c ExprC?] [srcloc source-location?]])
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

(define/contract (idC->id id #:ctxt [ctxt #f] #:props [props #f])
  [[idC?] [#:ctxt (or/c syntax? #f) #:props (or/c syntax? #f)] . ->* . identifier?]
  (match-define (idC sym srcloc) id)
  (datum->syntax ctxt sym (build-source-location-list srcloc) props))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; interp-expanded : ExprC Env -> Any
(define/contract (interp-expanded exprC env)
  [ExprC? env? . -> . any/c]
  (my-type-case ExprC exprC
    [(valC val _) val]
    [(letC `(,(and ps `[,ids ,vals]) ...) body srcloc)
     (define orig-env env)
     (when-let [id (check-duplicate-identifier (map idC->id ids))]
       (raise-syntax-error 'let "duplicate identifier" id))
     (interp-expanded
      body
      (for/fold ([env orig-env]) ([($ `[,(idC var _) ,val]) (in-list ps)])
        (hash-set env var (valC (interp-expanded val orig-env) srcloc))))]
    [(fnC ids body srcloc)
     (fnV (map idC-sym ids) body env)]
    [(idC (? symbol? sym) srcloc)
     (match (hash-ref env sym
              (λ () (raise-syntax-error 'sym "unbound identifier" (idC->id exprC))))
       [(valC val _) val])]
    [(appC f args srcloc)
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
    [(ifC a b c srcloc)
     (if (interp-expanded a env)
         (interp-expanded b env)
         (interp-expanded c env))]
    ))

(struct fnV (syms body env) #:transparent
  #:guard
  (lambda (syms body env _)
    (unless ((listof symbol?) syms)
      (error 'fnV "expected (listof symbol?), given: ~v" syms))
    (unless (ExprC? body)
      (error 'fnV "expected ExprC?, given: ~v" body))
    (unless (env? env)
      (error 'fnV "expected env?, given: ~v" env))
    (values syms body env)
    )
  #:property prop:procedure
  (keyword-lambda (kws kw-args this)
    (match-define (fnV syms body env) this)
    (define given-syms (map keyword->symbol kws))
    (for ([sym (in-list given-syms)])
      (unless (member sym syms)
        (error 'fnV "does not accept given keyword" (symbol->keyword sym))))
    (define rst-syms (remove* given-syms syms))
    (cond
      [(empty? rst-syms)
       (define/contract body-env env?
         (for/fold ([env env]) ([sym (in-list given-syms)]
                                [arg (in-list kw-args)])
           (hash-set env sym (valC arg #f))))
       (interp-expanded body body-env)]
      [(empty? given-syms) this]
      [else
       (fnV
        rst-syms
        body
        (for/fold ([env env]) ([sym (in-list given-syms)]
                               [arg (in-list kw-args)])
          (hash-set env sym (valC arg #f))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (mk-valC v)
  (valC v (quote-srcloc v)))

