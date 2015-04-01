#lang racket/base

(provide (all-defined-out))

(require racket/match
         racket/contract/base
         racket/contract/region
         racket/list
         my-plai/my-type-case
         "interp-expanded.rkt"
         "stuff.rkt"
         )
(module+ test
  (require rackunit
           ))

(define/contract (fnV-remove-unneeded f)
  [fnV? . -> . fnV?]
  (match-define (fnV syms body env) f)
  (define vars (find-free-vars body))
  (define new-syms
    (for/list ([sym (in-list syms)]
               #:when (member sym vars))
      sym))
  (define new-env
    (for/hasheq ([(k v) (in-hash env)]
                 #:when (member k vars)
                 #:unless (member k new-syms))
      (values k v)))
  (fnV new-syms body new-env))

(define/contract (fnV-remove-unneeded/substitute f)
  [fnV? . -> . fnV?]
  (match-define (fnV syms body env) (fnV-remove-unneeded f))
  (define new-body
    (substitute body (hash-remove* env syms)))
  (fnV syms new-body empty-env))

(define (->fnV f)
  (match f
    [(fnV _ _ _) f]
    [(? number? n) (fnV '() (valC n) empty-env)]))

(define/contract (exprC->fnV body)
  [ExprC? . -> . fnV?]
  (define syms (find-free-vars body))
  (fnV syms body empty-env))

(define/contract (find-free-vars e)
  [ExprC? . -> . (listof symbol?)]
  (my-type-case ExprC e
    [valC '()]
    [(idC v) (list v)]
    [(letC ps body)
     (remove-duplicates
      (flatten*
       (map find-free-vars (map second ps))
       (remove* (map first ps) (find-free-vars body))))]
    [(fnC syms body)
     (remove* syms (find-free-vars body))]
    [(appC f args)
     (remove-duplicates
      (flatten*
      (find-free-vars f)
      (for/list ([arg (in-list args)])
        (cond [(ExprC? arg) (find-free-vars arg)]
              [(keyword? arg) '()]
              [else (error "bad")]))))]
    [(ifC a b c)
     (remove-duplicates
      (append
       (find-free-vars a)
       (find-free-vars b)
       (find-free-vars c)))]
    ))

(define/contract (substitute body env)
  [ExprC? env? . -> . ExprC?]
  (substitute/ExprC body env))

(define/contract (substitute/ExprC body hsh)
  [ExprC? (ihasheq/c symbol? ExprC?) . -> . ExprC?]
  (my-type-case ExprC body
    [(idC x) (hash-ref hsh x (λ () body))]
    [(valC v) body]
    [(letC `([,xs ,vs] ...) body)
     (letC (map list xs (for/list ([v (in-list vs)]) (substitute/ExprC v hsh)))
           (substitute/ExprC body (hash-remove* hsh xs)))]
    [(fnC xs body)
     (fnC xs (substitute/ExprC body (hash-remove* hsh xs)))]
    [(appC f args)
     (appC (substitute/ExprC f hsh)
           (for/list ([arg (in-list args)])
             (match arg
               [(? ExprC? arg)
                (substitute/ExprC arg hsh)]
               [(list (? keyword? kw) (? ExprC? arg))
                (list kw (substitute/ExprC arg hsh))])))]
    [(ifC a b c)
     (ifC (substitute/ExprC a hsh)
          (substitute/ExprC b hsh)
          (substitute/ExprC c hsh))]
    ))

(define/contract (appC/substitute f args)
  [ExprC? (listof (or/c ExprC? (list/c keyword? ExprC?))) . -> . ExprC?]
  (define e (appC f args))
  (my-type-case ExprC f
    [(valC v)
     (cond
       [(fnV? v)
        (define f (fnV-remove-unneeded/substitute v))
        (match-define (fnV syms body env) f)
        (match args
          [(list (list (? keyword? kws) (and kw-args (or (valC _) (idC _)))) ...)
           (substitute/ExprC
            body
            (for/hasheq ([kw (in-list kws)]
                         [kw-arg (in-list kw-args)])
              (values (keyword->symbol kw) kw-arg)))]
          [_ (appC (valC f) args)])]
       [else e])]
    [else e]))

