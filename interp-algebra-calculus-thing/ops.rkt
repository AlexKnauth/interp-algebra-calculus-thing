#lang racket/base

(provide (all-defined-out))

(require racket/match
         racket/contract/base
         racket/contract/region
         racket/list
         math/base
         syntax/parse/define
         my-plai/my-type-case
         match-string
         (prefix-in rkt: racket/base)
         "interp-expanded.rkt"
         "fnV-utils.rkt"
         "stuff.rkt"
         (for-syntax racket/base
                     syntax/parse
                     ))
(module+ test
  (require rackunit kw-utils/kw-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (freevar sym)
  [symbol? . -> . fnV?]
  (fnV (list sym) (idC sym #f) empty-env))

(define-simple-macro (define-freevars x:id ...)
  (begin (define x (freevar 'x)) ...))

(define-simple-macro (let-freevars (x:id ...) body:expr ...+)
  (let ([x (freevar 'x)] ...) body ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (fn-args args*)
  [(listof (or/c number? fnV?)) . -> . (or/c (listof number?) fnV?)]
  (define args (map maybe-thunk->number args*))
  (cond
    [(andmap number? args) args]
    [else
     (define syms
       (remove-duplicates
        (flatten (map fnV-syms (filter fnV? args)))))
     (fnV syms
          (appC (mk-valC list)
                (for/list ([arg (in-list args)])
                  (cond
                    [(number? arg) (valC arg #f)]
                    [else
                     (appC/substitute
                      (valC arg #f)
                      (for/list ([sym (in-list (fnV-syms arg))])
                        (list (symbol->keyword sym)
                              (idC sym #f))))]))
                #f)
          empty-env)]))

(define (fn-op name op op/exprs)
  (define (new-op . args)
    (match (fn-args args)
      [(list args ...)
       (apply op args)]
      [(fnV syms (appC (valC (== list) _) arg-exprs _) env)
       (fnV syms (op/exprs arg-exprs) env)]))
  (procedure-reduce-arity
   (procedure-rename new-op name)
   (procedure-arity op)))

(define/contract (maybe-thunk->number x)
  [(or/c number? fnV?) . -> . (or/c number? fnV?)]
  (cond
    [(number? x) x]
    [(fnV? x)
     (define f (fnV-remove-unneeded x))
     (cond [(empty? (fnV-syms f)) (maybe-thunk->number (f))]
           [else f])]
    [else (error 'maybe-thunk->number "expected (or/c number? fnV?), given: ~v" x)]))

(define (numC? x)
  (match x
    [(valC (? number? n) _) #t]
    [_ #f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract +
  [[] #:rest (listof (or/c number? fnV?)) . ->* . (or/c number? fnV?)]
  (fn-op
   '+
   (λ args (sum args))
   (λ (args)
     (define-values (nums exprs)
       (partition numC? args))
     (define n (sum (map valC-val nums)))
     (cond [(zero? n) (appC (mk-valC +) exprs #f)]
           [else      (appC (mk-valC +) (append exprs (list (valC n #f))) #f)]))))

(define/contract *
  [[] #:rest (listof (or/c number? fnV?)) . ->* . (or/c number? fnV?)]
  (fn-op
   '*
   (λ args (product args))
   (λ (args)
     (define-values (nums exprs)
       (partition numC? args))
     (define n (product (map valC-val nums)))
     (cond [(= n 1) (appC (mk-valC *) exprs #f)]
           [else    (appC (mk-valC *) (append (list (valC n #f)) exprs) #f)]))))

(define/contract ^
  [(or/c number? fnV?) (or/c number? fnV?) . -> . (or/c number? fnV?)]
  (fn-op '^ expt (λ (args) (appC (mk-valC ^) args #f))))

(define/contract sin
  [(or/c number? fnV?) . -> . (or/c number? fnV?)]
  (fn-op 'sin rkt:sin (λ (args) (appC (mk-valC sin) args #f))))

(define/contract cos
  [(or/c number? fnV?) . -> . (or/c number? fnV?)]
  (fn-op 'cos rkt:cos (λ (args) (appC (mk-valC cos) args #f))))

(define/contract ln
  [(or/c number? fnV?) . -> . (or/c number? fnV?)]
  (fn-op 'ln rkt:log (λ (args) (appC (mk-valC ln) args #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract -
  (case->
   [(or/c number? fnV?) . -> . (or/c number? fnV?)]
   [(or/c number? fnV?) #:rest (listof (or/c number? fnV?)) . -> . (or/c number? fnV?)])
  (case-lambda
    [(a) (* -1 a)]
    [(a b . rst) (+ a (- (apply + b rst)))]))

(define/contract (1/ x)
  [(or/c number? fnV?) . -> . (or/c number? fnV?)]
  (^ x -1))

(define/contract /
  (case->
   [(or/c number? fnV?) . -> . (or/c number? fnV?)]
   [(or/c number? fnV?) #:rest (listof (or/c number? fnV?)) . -> . (or/c number? fnV?)])
  (case-lambda
    [(a) (1/ a)]
    [(a b . rst) (* a (1/ (apply * b rst)))]))

(define-simple-macro (fn (x:id ...) body:expr)
  (let-freevars (x ...) (fn-helper '(x ...) body)))

(define (fn-helper syms f)
  (match f
    [(fnV f-syms body env)
     (define s (remove-duplicates (append syms f-syms)))
     (fnV s body env)]
    [(? number? x)
     (fnV syms (valC x #f) empty-env)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (+? f)
  (match f
    [(fnV _ (appC (valC (== +) _) (list (? ExprC?) ...) _) _) #t]
    [_ #f]))

(define (*? f)
  (match f
    [(fnV _ (appC (valC (== *) _) (list (? ExprC?) ...) _) _) #t]
    [_ #f]))

(define (^? f)
  (match f
    [(fnV _ (appC (valC (== ^) _) (list (? ExprC?) (? ExprC?)) _) _) #t]
    [_ #f]))

(define (sin? f)
  (match f
    [(fnV _ (appC (valC (== sin) _) (list (? ExprC?)) _) _) #t]
    [_ #f]))

(define (cos? f)
  (match f
    [(fnV _ (appC (valC (== cos) _) (list (? ExprC?)) _) _) #t]
    [_ #f]))

(define (ln? f)
  (match f
    [(fnV _ (appC (valC (== ln) _) (list (? ExprC?)) _) _) #t]
    [_ #f]))

(define (+-args f)
  (match-define (fnV syms (appC (valC (== +) _) arg-exprs _) env) f)
  (for/list ([arg-expr (in-list arg-exprs)])
    (my-type-case ExprC arg-expr
      [(valC v _)
       (match v
         [(? number? v) v])]
      [else
       (fnV syms arg-expr env)])))

(define (*-args f)
  (match-define (fnV syms (appC (valC (== *) _) arg-exprs _) env) f)
  (for/list ([arg-expr (in-list arg-exprs)])
    (my-type-case ExprC arg-expr
      [(valC v _)
       (match v
         [(? number? v) v])]
      [else
       (fnV syms arg-expr env)])))

(define (^-args f)
  (match-define (fnV syms (appC (valC (== ^) _) arg-exprs _) env) f)
  (for/list ([arg-expr (in-list arg-exprs)])
    (my-type-case ExprC arg-expr
      [(valC v _)
       (match v
         [(? number? v) v])]
      [else
       (fnV syms arg-expr env)])))

(define (sin-args f)
  (match-define (fnV syms (appC (valC (== sin) _) arg-exprs _) env) f)
  (for/list ([arg-expr (in-list arg-exprs)])
    (my-type-case ExprC arg-expr
      [(valC v _)
       (match v
         [(? number? v) v])]
      [else
       (fnV syms arg-expr env)])))

(define (cos-args f)
  (match-define (fnV syms (appC (valC (== cos) _) arg-exprs _) env) f)
  (for/list ([arg-expr (in-list arg-exprs)])
    (my-type-case ExprC arg-expr
      [(valC v _)
       (match v
         [(? number? v) v])]
      [else
       (fnV syms arg-expr env)])))

(define (ln-args f)
  (match-define (fnV syms (appC (valC (== ln) _) arg-exprs _) env) f)
  (for/list ([arg-expr (in-list arg-exprs)])
    (my-type-case ExprC arg-expr
      [(valC v _)
       (match v
         [(? number? v) v])]
      [else
       (fnV syms arg-expr env)])))

(define-match-expander +:
  (syntax-parser
    [(+: pat:expr ...)
     #'(? +? (app +-args (list-no-order pat ...)))]))

(define-match-expander *:
  (syntax-parser
    [(*: pat:expr ...)
     #'(? *? (app *-args (list-no-order pat ...)))]))

(define-match-expander ^:
  (syntax-parser
    [(^: pat1:expr pat2:expr)
     #'(? ^? (app ^-args (list pat1 pat2)))]))

(define-match-expander sin:
  (syntax-parser
    [(sin: pat:expr)
     #'(? sin? (app sin-args (list pat)))]))

(define-match-expander cos:
  (syntax-parser
    [(cos: pat:expr)
     #'(? cos? (app cos-args (list pat)))]))

(define-match-expander ln:
  (syntax-parser
    [(ln: pat:expr)
     #'(? ln? (app ln-args (list pat)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (extend-env/ops.rkt env)
  (hash-set* env
             '+ (mk-valC +)
             '* (mk-valC *)
             '^ (mk-valC ^)
             '- (mk-valC -)
             '1/ (mk-valC 1/)
             '/ (mk-valC /)
             'sin (mk-valC sin)
             'cos (mk-valC cos)
             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (define init-env (extend-env/ops.rkt empty-env))
  (test-case "+, *, and ^ on numbers"
    (check-equal? (+ 1 2) 3)
    (check-equal? (* 2 3) 6)
    (check-equal? (^ 2 3) 8)
    )
  (define-freevars x y)
  (test-case "+, *, and ^ and `freevar`s"
    (check-equal? ((+ x 1 y 2 3) #:x 1 #:y 2) 9)
    (check-equal? (+ x 1 y 2 3) (+ x y 6))
    (define 2x+1 (+ (* 2 x) 1))
    (check-equal? (2x+1 #:x 0) 1)
    (check-equal? (2x+1 #:x 1) 3)
    (check-equal? (2x+1 #:x 2) 5)
    (check-equal? (map 2x+1 #:x '(0 1 2)) '(1 3 5))
    (define x^2+1 (+ (^ x 2) 1))
    (check-equal? (x^2+1 #:x 0) 1)
    (check-equal? (x^2+1 #:x 1) 2)
    (check-equal? (x^2+1 #:x 2) 5)
    (check-equal? (x^2+1 #:x 3) 10)
    (check-equal? (x^2+1 #:x 4) 17)
    (check-equal? (map x^2+1 #:x '(0 1 2 3 4)) '(1 2 5 10 17))
    )
  (define fnx:x (fn (x) x))
  (define fny:y (fn (y) y))
  (define fn:3  (fn () 3))
  (check-equal? (+ fnx:x 1 fny:y 2 fn:3)
                (+ x y 6))
  (check-equal? (+ fnx:x 1 fny:y 2 fn:3)
                (fn (x y) (+ x y 6)))
  (check-equal? ((+ fnx:x 1 fny:y 2 fn:3) #:x 1 #:y 2)
                9)
  (check-equal? (let ([f (let ([x 1]) (fn () x))]
                      [g (fn (x) x)])
                  ((+ g f) #:x 2))
                3)
  )
