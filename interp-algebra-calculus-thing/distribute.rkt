#lang racket/base

(provide distribute)

(require racket/match
         "ops.rkt"
         "ops-2.rkt"
         )
(module+ test
  (require rackunit))

;; distribute : (or/c Number FnV) -> (or/c Number FnV)
(module+ test
  (test-case "distribute"
    (define-freevars a b c d)
    (check-equal? (distribute (* 2 (+ a b))) (+ (* 2 a) (* 2 b)))
    (check-equal? (distribute (* c (+ a b))) (+ (* c a) (* c b)))
    (check-equal? (distribute (* (+ a b) c)) (+ (* a c) (* b c)))
    (check-equal? (distribute (* (+ a b) (+ c d))) (+ (* a c) (* a d) (* b c) (* b d)))
    ))

(define (distribute e)
  (match e
    [(? number? n) n]
    [(? *? (app *-args args))
     (define termss ; (* (+ a ...) ...)
       (for/list ([a (in-list args)])
         (match a
           [0 '()] ; 0 -> (+)
           [(? +? (app +-args args)) args]
           [a (list a)] ; a -> (+ a)
           )))
     (+_ (for/list ([terms (in-list (foil-helper termss))])
           (*_ terms)))]
    [_ e]))

;; foil-helper : (Listof (Listof A)) -> (Listof (Listof A))
(module+ test
  (test-case "foil-helper"
    ;; (*) -> (+ (*))
    (check-equal? (foil-helper '()) '(()))
    ;; (* (+) (+ a b c)) -> (+)
    (check-equal? (foil-helper '(() (a b c))) '())
    ;; (* (+ a b c) (+)) -> (+)
    (check-equal? (foil-helper '((a b c) ())) '())
    ;; (* (+ a) (+ b)) -> (+ (* a b))
    (check-equal? (foil-helper '((a) (b))) '((a b)))
    ;; (* (+ a) (+ b c)) -> (+ (* a b) (* a c))
    (check-equal? (foil-helper '((a) (b c))) '((a b) (a c)))
    ;; (* (+ a b) (+ c d)) -> (+ (* a c) (* a d) (* b c) (* b d))
    (check-equal? (foil-helper '((a b) (c d))) '((a c) (a d) (b c) (b d)))
    ))

(define (foil-helper xss)
  (match xss
    [(list) '(())] ; (*) -> (+ (*))
    [(cons fst rst)
     (for*/list ([x (in-list fst)]
                 [xs (in-list (foil-helper rst))])
       (cons x xs))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

