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
         "stuff.rkt"
         )
(module+ test
  (require rackunit syntax/parse/define))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gen-C)
  (string->symbol (symbol->string (datum-intern-literal (gensym 'C)))))

(define/contract (integral f var [C-sym (gen-C)])
  [(or/c fnV? number?) symbol? symbol? . -> . fnV?]
  (define g (fnV-remove-unneeded/substitute (->fnV f)))
  (match-define (fnV syms body env) g)
  (when (member C-sym syms)
    (error 'integral "~a is an argument to the function ~v" C-sym f))
  (when (equal? C-sym var)
    (error 'integral "~a cannot be both the integration variable and the constant" C-sym))
  (define vars (remove* (hash-keys env) (find-free-vars (fnC syms body))))
  (unless (empty? vars) (error 'fnV "unbound-identifiers: ~a" vars))
  (define x (freevar var))
  (define C (freevar C-sym))
  (define G
    (match (maybe-thunk->number g)
      [(? number? n) (*_ n x)] ; + C added later
      [(fnV _ (idC (== var)) _) (*_ 1/2 (^_ x 2))]
      [(fnV _ (idC (and n (not (== var)))) _) (*_ (freevar n) x)]
      [(+: args ...)
       (+_ (for/list ([arg (in-list args)])
             (integral/!C arg var)))]
      [(*: (? number? n) rst ...)
       (*_ n (integral/!C (*_ rst) var))]
      [(*: args ...)
       (error 'integral "....")]
      [(^: (== x) (? number? (and n (not -1))))
       (define n+1 (+_ n 1))
       (*_ (1/ n+1) (^_ x n+1))]
      [(^: (== x) -1)
       (ln x)]
      [(sin: (== x))
       (*_ -1 (cos x))]
      [(cos: (== x))
       (sin x)]
      [g (error 'partial-deriv ".... g: ~v" g)]))
  (match (+_ G C)
    [(fnV G-syms body env)
     (fnV-remove-unneeded (fnV (remove-duplicates (append syms G-syms)) body env))]))

(define (integral/!C f var)
  (define C-sym (gen-C))
  (keyword-apply (integral f var C-sym) (list (symbol->keyword C-sym)) (list 0) '()))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (define-simple-macro
    (int x body)
    (integral (fn (x) body) 'x 'C))
  (check-equal? (int x 0) (fn (C) C))
  (check-equal? (int x 1) (fn (x C) (+ x C)))
  (check-equal? (int x 2) (fn (x C) (+ (* 2 x) C)))
  (check-equal? (int x x) (fn (x C) (+ (* 1/2 (^ x 2)) C)))
  (check-equal? (int x (* 2 x)) (fn (x C) (+ (^ x 2) C)))
  (check-equal? (int x (* 3 x)) (fn (x C) (+ (* 3/2 (^ x 2)) C)))
  (check-equal? (int x (+ x 1)) (fn (x C) (+ (* 1/2 (^ x 2)) x C)))
  (check-equal? (int x (+ x x)) (fn (x C) (+ (* 1/2 (^ x 2)) (* 1/2 (^ x 2)) C)))
  (check-equal? (int x (^ x 2)) (fn (x C) (+ (* 1/3 (^ x 3)) C)))
  (check-equal? (int x (^ x 3)) (fn (x C) (+ (* 1/4 (^ x 4)) C)))
  (check-equal? (int x (sin x)) (fn (x C) (+ (- (cos x)) C)))
  (check-equal? (int x (cos x)) (fn (x C) (+ (sin x) C)))
  (check-equal? (int x (1/ x)) (fn (x C) (+ (ln x) C)))
  )
