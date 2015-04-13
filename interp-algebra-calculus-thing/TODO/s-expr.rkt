#lang racket/base

(provide exprC->s-expr+env)

(require racket/match
         unstable/match
         my-cond
         my-plai/my-type-case
         "../interp-expanded.rkt"
         )
(module+ test
  (require testing-utils syntax/parse/define))

;; exprC->s-expr+env : ExprC Env -> (values ExprC Env)
(define (exprC->s-expr+env e env)
  (my-type-case ExprC e
    [(idC sym srcloc)
     (cond [(hash-has-key? env sym) (values sym env)]
           [else (error 'exprC->s-expr "unbound identifier")])]
    [(valC val srcloc)
     (my-cond
       [(or (number? val) (boolean? val) (string? val)) (values val env)]
       #:def (define k
               (for/first ([(k v) (in-hash env)] #:when (match? v (valC (== val) _)))
                 k))
       [k (values k env)]
       #:def (define k (object-name val))
       [(and (symbol? k) (not (hash-has-key? env k)))
        (values k (hash-set env k e))]
       [else (values e env)])]
    [(appC f args srcloc)
     (define-values (f-expr f-env) (exprC->s-expr+env f env))
     (define-values (rev-arg-exprs new-env)
       (for/fold ([rev-arg-exprs '()] [env f-env]) ([arg (in-list args)])
         (match arg
           [(? ExprC? arg)
            (define-values (arg-expr arg-env) (exprC->s-expr+env arg env))
            (values (list* arg-expr rev-arg-exprs) arg-env)]
           [(list (? keyword? kw) (? ExprC? arg))
            (define-values (arg-expr arg-env) (exprC->s-expr+env arg env))
            (values (list* arg-expr kw rev-arg-exprs) arg-env)])))
     (values (cons f-expr (reverse rev-arg-exprs)) new-env)]
    [(letC vars body srcloc)
     (error 'exprC->s-expr+env "....")]
    [(fnC ids body srcloc)
     (error 'exprC->s-expr+env "....")]
    [(ifC a b c srcloc)
     (error 'exprC->s-expr+env "....")]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (define-simple-macro (vs->lst expr:expr)
    (call-with-values (λ () expr) list))
  (define-simple-macro (chk (~and c [[exprC:expr env1:expr] [s-expr:expr env2:expr]]) ...)
    (begin (with-loc c
             (check-equal? (vs->lst (exprC->s-expr+env exprC env1)) (list s-expr env2)))
           ...))
  (chk
   [[(valC 1 #f) empty-env] [1 empty-env]]
   [[(appC (valC + #f) (list (valC 1 #f) (valC 2 #f)) #f) empty-env]
    ['(+ 1 2) (hasheq '+ (valC + #f))]]
   [[(appC (valC + #f) (list (list '#:x (valC 1 #f)) (valC 2 #f)) #f) empty-env]
    ['(+ #:x 1 2) (hasheq '+ (valC + #f))]]
   [[(appC (valC + #f) (list (valC 1 #f) (valC 2 #f)) #f) (hasheq '+ (valC string-append #f))]
    [`(,(valC + #f) 1 2) (hasheq '+ (valC string-append #f))]]
   )
  )

