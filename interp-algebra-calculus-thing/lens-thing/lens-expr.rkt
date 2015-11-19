#lang sweet-exp racket

require fancy-app
        lens
        prefix-in rkt: racket/base
        prefix-in rkt: math/base
        unstable/lens
module+ test
  require rackunit

;; Expr is one of:
;;  - Number
;;  - String
;;  - (Lens Env Expr)
;; Env is (Hashof Symbol Expr)
(define (env-lookup env sym)
  (hash-ref env sym))
(define (env-set env sym v)
  (hash-set env sym v))
(define (make-env assoc)
  (make-immutable-hash assoc))

;; freevar : Symbol -> Expr
(define (freevar sym)
  (make-lens
   (lambda (env)
     (env-lookup env sym))
   (lambda (env new-res)
     (env-set env sym new-res))))

;; sum : (Listof Expr) -> Expr
(define (sum exprs)
  (define-values [nums lenses]
    (partition number? exprs))
  (define num (rkt:sum nums))
  (unless (andmap lens? lenses)
    (error "sum expects number expressions"))
  (cond [(empty? lenses)
         num]
        [else
         (make-lens
          (lambda (env)
            (sum (cons num (map (lens-view _ env) lenses))))
          (lambda (env new-res)
            (match lenses
              [(list) (error "....")]
              [(list lens)
               (lens-set lens env (+ new-res (rkt:- num)))]
              [_ (error "....")])))]))

;; + : Expr ... -> Expr
(define (+ . exprs)
  (sum exprs))

;; app : Expr Env -> Expr
(define (app expr env)
  (cond [(number? expr) expr]
        [(string? expr) expr]
        [else (lens-view expr env)]))


module+ test
  (define x (freevar 'x))
  (define y (freevar 'y))
  (define (x= v) (make-env `([x . ,v])))
  (define (y= v) (make-env `([y . ,v])))
  (define x=3 (x= 3))
  (define y=4 (y= 4))
  (define x=3∧y=4 (make-env '([x . 3] [y . 4])))
  (check-equal? (app x x=3) 3)
  (check-equal? (app (+ x 1) x=3) 4)
  (check-equal? (app (+ x y) x=3∧y=4) 7)
  (check-equal? (lens-set (+ x 1) x=3 5) (x= 4))
  (define-check (check-equal-at? l1 l2 seq)
    (for ([env seq])
      (check-equal? (app l1 env) (app l2 env))))
  (check-equal-at? (env-lookup (lens-set (+ x 1) x=3 y) 'x)
                   (env-lookup (x= (+ y -1)) 'x)
                   (for/list ([v (in-range -10 11)])
                     (y= v)))

