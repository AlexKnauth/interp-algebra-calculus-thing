#lang racket/base

(provide (all-defined-out))

(require racket/match
         racket/contract/base
         racket/contract/region
         racket/list
         my-plai/my-type-case
         "interp-expanded.rkt"
         "fnV-utils.rkt"
         "ops.rkt"
         "ops-2.rkt"
         )
(module+ test
  (require rackunit syntax/parse/define))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (partial-deriv f var)
  [(or/c fnV? number?) symbol? . -> . fnV?]
  (define g (fnV-remove-unneeded/substitute (->fnV f)))
  (match-define (fnV syms body env) g)
  (define vars (remove* (hash-keys env) (find-free-vars (fnC syms body))))
  (unless (empty? vars) (error 'fnV "unbound-identifiers: ~a" vars))
  (cond
    [(not (member var syms)) (fnV '() (valC 0) empty-env)]
    [else
     (define g*
       (match (maybe-thunk->number g)
         [(? number?) 0]
         [(fnV _ (idC (== var)) _) 1]
         [(fnV _ (idC (not (== var))) _) 0]
         [(+: args ...)
          (+_ (for/list ([arg (in-list args)])
                (partial-deriv arg var)))]
         [(*: args ...)
          (+_ (for/list ([arg (in-list args)])
                (define deriv (maybe-thunk->number (partial-deriv arg var)))
                (define other-args (remq arg args))
                (*_ deriv other-args)))]
         [(^: b n)
          (define b* (maybe-thunk->number (partial-deriv b var)))
          (define n* (maybe-thunk->number (partial-deriv n var)))
          (cond [(equal? n* 0)
                 (let ([n (maybe-thunk->number n)])
                   (*_ n b* (^_ b (- n 1))))]
                [else (error 'partial-deriv ".... n*: ~v" n*)])]
         [(sin: u)
          (define u* (maybe-thunk->number (partial-deriv u var)))
          (*_ u* (cos u))]
         [(cos: u)
          (define u* (maybe-thunk->number (partial-deriv u var)))
          (*_ -1 u* (sin u))]
         [(ln: u)
          (define u* (maybe-thunk->number (partial-deriv u var)))
          (*_ u* (^_ u -1))]
         [g (error 'partial-deriv ".... g: ~v" g)]))
     (match g*
       [(fnV g*-syms body env)
        (fnV-remove-unneeded (fnV (remove-duplicates (append syms g*-syms)) body env))]
       [(? number? n)
        (->fnV n)])]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (define-simple-macro
    (deriv x body)
    (partial-deriv (fn (x) body) 'x))
  (check-equal? (deriv x 0) (fn () 0))
  (check-equal? (deriv x 1) (fn () 0))
  (check-equal? (deriv x 2) (fn () 0))
  (check-equal? (deriv x x) (fn () 1))
  (check-equal? (deriv x (+ x 1)) (fn () 1))
  (check-equal? (deriv x (+ x x)) (fn () 2))
  (check-equal? (deriv x (* 2 x)) (fn () 2))
  (check-equal? (deriv x (^ x 2)) (fn (x) (* 2 x)))
  (check-equal? (deriv x (^ x 3)) (fn (x) (* 3 (^ x 2))))
  (check-equal? (deriv x (^ (* 2 x) 3)) (fn (x) (* 6 (^ (* 2 x) 2))))
  (define-freevars n)
  (check-equal? (deriv x (^ x n)) (fn (x n) (* n (^ x (- n 1)))))
  (check-equal? (deriv x (sin x)) (fn (x) (cos x)))
  (check-equal? (deriv x (cos x)) (fn (x) (- (sin x))))
  (check-equal? (deriv x (sin (* 2 x))) (fn (x) (* 2 (cos (* 2 x)))))
  (check-equal? (deriv x (cos (* 2 x))) (fn (x) (* -2 (sin (* 2 x)))))
  (check-equal? (deriv x (ln x)) (fn (x) (1/ x)))
  (check-equal? (deriv x (ln (* 2 x))) (fn (x) (1/ x)))
  (check-equal? (deriv x (ln (^ x 2))) (fn (x) (* 2 x (^ x -2)))) ; should it try to simplify further?
  (check-equal? (deriv x (* 2 (ln x))) (fn (x) (* 2 (1/ x))))
  )
