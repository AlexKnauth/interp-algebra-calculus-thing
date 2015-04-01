#lang racket/base

(provide (all-defined-out))

(require racket/match
         racket/list
         "ops.rkt"
         )

(define (+_ . args)
  (match (apply + (flatten args))
    [(+: a) a]
    [(+: (+: a ...) rst ...)
     (+_ a rst)]
    [x x]))

(define (*_ . args)
  (match (apply * (flatten args))
    [(*: a) a]
    [(*: 0 rst ...) 0]
    [(*: (*: a ...) rst ...)
     (*_ a rst)]
    [(*: (+: 0 a ...) rst ...)
     (*_ (+_ a) rst)]
    [(*: (^: (*: a ...) -1) rst ...)
     (*_ (for/list ([a (in-list a)]) (^_ a -1)) rst)]
    ;[(and product (*: args ...))
    ; (define args/bn
    ;   (for/list ([arg (in-list args)])
    ;     (match arg
    ;       [(^: b n) (list b n)]
    ;       [b (list b 1)])))
    ; (define bs (map first args/bn))
    ; (define bs-2
    ;   (remove-duplicates bs))
    ; (cond [(equal? bs bs-2) product]
    ;       [else
    ;        (define args/bn-2
    ;          (for/list ([b (in-list bs-2)])
    ;            (list b (+_ (for/list ([bn (in-list args/bn)]
    ;                                   #:when (equal? (first bn) b))
    ;                          (second bn))))))
    ;        (*_ (for/list ([bn (in-list args/bn-2)])
    ;              (match bn
    ;                [(list b n) (^_ b n)])))])]
    [x x]))

(define (^_ b n)
  (match (^ b n)
    [(^: 0 0) 1]
    [(^: b 0) 1]
    [(^: b 1) b]
    [(^: (^: b n1) n2)
     (^_ b (* n1 n2))]
    [x x]))

