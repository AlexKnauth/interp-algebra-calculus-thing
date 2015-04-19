#lang racket/base

(provide check-duplicate-identifier
         bound-identifier=?
         )

(require racket/match
         racket/bool
         racket/set
         "syntax-object.rkt"
         "syntax.rkt"
         "ExprC.rkt"
         )

;; rewritten from racket/private/stxcase-cheme.rkt
(define (check-duplicate-identifier names)
  (unless (and (list? names) (andmap id-or-idC? names))
    (raise-argument-error 'check-duplicate-identifier "(listof identifier?)" names))
  (let/ec escape
    (let ([ht (make-hasheq)])
      (for ([defined-name (in-list names)])
        (define defined-name* (id->idC defined-name))
        (match-define (idC sym scopes srcloc) defined-name*)
        (let ([l (hash-ref ht sym null)])
          (when (ormap (lambda (i) (bound-identifier=? i defined-name*)) l)
            (escape defined-name*))
          (hash-set! ht sym (cons defined-name* l))))
      #f)))

(define (bound-identifier=? id1 id2)
  (match-define (idC sym1 scopes1 _) (id->idC id1))
  (match-define (idC sym2 scopes2 _) (id->idC id2))
  (and (symbol=? sym1 sym2)
       (set=? scopes1 scopes2)))

