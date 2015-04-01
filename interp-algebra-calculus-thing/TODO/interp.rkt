#lang racket/base

(provide (all-defined-out))

(require racket/match
         racket/contract/base
         racket/list
         unstable/list
         generic-bind/as-rkt-names
         rackjure/conditionals
         my-plai/my-type-case
         "../interp-expanded.rkt"
         )
(module+ test
  (require rackunit
           ))

;; an ExprS is either an ExprS-struct or a non-keyword s-expression
(define ExprS? (not/c keyword?))
(define-type ExprS-struct
  [letS [vars (listof (list/c symbol? ExprS?))] [body ExprS?]]
  [fnS [syms (listof symbol?)] [body ExprS?]]
  [appS [f ExprS?] [args (listof (or/c ExprS? keyword?))]]
  [ifS [a ExprS?] [b ExprS?] [c ExprS?]]
  [letstxS [vars (listof (list/c symbol? ExprS?))] [body ExprS?]]
  )


;; interp : S-Exp Env -> Any
(define/contract (interp s-exp env)
  [any/c env? . -> . any/c]
  (interp-expanded (expand s-exp env) env))

;; expand : S-Exp Env -> ExprC
(define/contract (expand s-exp env)
  [any/c env? . -> . ExprC?]
  (match s-exp
    [(? ExprC? e) e]
    [(? ExprS-struct? e)
     (my-type-case ExprS-struct e
       [(letS `(,ps ...) body)
        (when-let [it (check-duplicate (map first ps))]
          (error 'let "duplicate identifier: ~a" it))
        (letC
         (for/list ([($ `[,x ,y]) (in-list ps)])
           `[,x ,(expand y env)])
         (expand body env))]
       [(letstxS `(,ps ...) body)
        (define orig-env env)
        (when-let [it (check-duplicate (map first ps))]
          (error 'let-syntax "duplicate identifier: ~a" it))
        (expand
         body
         (for/fold ([env orig-env]) ([($ `[,x ,y]) (in-list ps)])
           (hash-set env x (syntax-binding (interp-expanded (expand y empty-env) empty-env)))))]
       [(fnS `(,syms ...) body)
        (when-let [it (check-duplicate syms)]
          (error 'lambda "duplicate identifier: ~a" it))
        (fnC syms (expand body env))]
       [(appS f args)
        (appC (expand f env)
              (let loop ([rev-args '()] [args args])
                (match args
                  [(list) (reverse rev-args)]
                  [(list-rest (? keyword? kw) (and arg (not (? keyword?))) rst)
                   (loop (cons (list kw (expand arg env)) rev-args) rst)]
                  [(list-rest (and arg (not (? keyword?))) rst)
                   (loop (cons (expand arg env) rev-args) rst)])))]
       [(ifS a b c)
        (ifC (expand a env) (expand b env) (expand c env))])]
    [(? number? num) (valC num)]
    [(? boolean? b) (valC b)]
    [(? fnV? f) (valC f)]
    [(? symbol? sym)                              ; TODO: make it hygienic
     (match (hash-ref env sym #f)
       [(or (valC _) #f) (idC sym)]
       [(syntax-binding (? procedure? proc))
        (expand (proc #:stx s-exp) env)])]
    [`(,(? symbol? m) . ,_)                       ; TODO: make it hygienic
     #:when (syntax-binding? (hash-ref env m #f))
     (match (hash-ref env m)
       [(syntax-binding (? procedure? proc))
        (expand (proc #:stx s-exp) env)])]
    [`(,(and f (not (? keyword?))) ,args ...)
     (expand `(#%app ,f . ,args) env)]
    ))



(define (s-exp-fn syms body [env empty-env])
  (interp-expanded (expand (fnS syms body) env) env))

(define/contract (exprC->s-exp e)
  [ExprC? . -> . ExprS?]
  (my-type-case ExprC e
    [(valC x) (error "....")]
    [(idC x) x]
    [(letC vars body)
     (error "....")]
    [(fnC syms body)
     (error "....")]
    [(appC f args)
     (error "....")]
    [(ifC a b c)
     (error "....")]
    ))

(define/contract (extend-env/basic-stx env)
  [env? . -> . env?]
  (hash-set* env
             '#%app (syntax-binding
                     (lambda (#:stx s-exp)
                       (match s-exp
                         [`(#%app ,f . ,args)
                          (appS f args)])))
             'let (syntax-binding
                   (lambda (#:stx s-exp)
                     (match s-exp
                       [`(let ,vars ,body)
                        (letS vars body)])))
             'fn (syntax-binding
                  (lambda (#:stx s-exp)
                    (match s-exp
                      [`(fn ,syms ,body)
                       (fnS syms body)])))
             'if (syntax-binding
                  (lambda (#:stx s-exp)
                    (match s-exp
                      [`(if ,a ,b ,c)
                       (ifS a b c)])))
             ))

(define/contract (extend-env/vals env)
  [env? . -> . env?]
  (hash-set* env
             '+ (valC +)
             '- (valC -)
             '* (valC *)
             '/ (valC /)
             '^ (valC expt)
             ))

(define/contract (extend-env/interp.rkt env)
  [env? . -> . env?]
  (extend-env/vals (extend-env/basic-stx env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (define basic-stx-env (extend-env/basic-stx empty-env))
  (define init-env (extend-env/interp.rkt empty-env))
  (check-equal? (interp 1 empty-env) 1)
  (check-equal? (interp (valC '(+ 1 2)) empty-env) '(+ 1 2))
  (check-equal? (interp `(let ([x 1]) x) basic-stx-env) 1)
  (check-equal? (interp `(let ([x 1]) (let ([x 2]) x)) basic-stx-env) 2)
  (check-exn exn:fail? (λ () (expand `(let ([x 1] [x 1]) 1) basic-stx-env)))
  (check-exn exn:fail? (λ () (expand `(fn (x x) 1) basic-stx-env)))
  (check-equal? (interp `(+ 1 2) init-env) 3)
  (check-equal? (interp `(if #t 1 2) basic-stx-env) 1)
  (check-equal? (interp `(if #f 1 2) basic-stx-env) 2)
  (test-case "f(x) = x"
    (define f (interp `(fn (x) x) basic-stx-env))
    (check-equal? (f) f)
    (check-equal? (f #:x 1) 1)
    (check-equal? (interp `(let ([f (fn (x) x)]) (f #:x 1))
                          basic-stx-env)
                  1)
    )
  (test-case "f(x,y) = x"
    (define f (interp `(fn (x y) x) basic-stx-env))
    (check-equal? (f) f)
    (check-equal? (f #:x 1) (interp `(fn (y) x) (hash-set basic-stx-env 'x (valC 1))))
    (check-equal? (f #:y 2) (interp `(fn (x) x) (hash-set basic-stx-env 'y (valC 2))))
    (check-equal? (f #:x 1 #:y 2) 1)
    (check-equal? ((f #:x 1) #:y 2) 1)
    (check-equal? ((f #:y 2) #:x 1) 1)
    )
  (test-case "(let ([x 1]) (fn () x))"
    (define f
      (interp `(let ([x 1]) (fn () x)) basic-stx-env))
    (check-equal? (f) 1)
    (define g
      (interp `(let ([x 1]) (fn (x) x)) basic-stx-env))
    (check-equal? (g #:x 2) 2)
    (check-equal? (interp `(let ([f (let ([x 1])
                                      (fn () x))])
                             (let ([x 2])
                               (f)))
                          basic-stx-env)
                  1)
    )
  )
