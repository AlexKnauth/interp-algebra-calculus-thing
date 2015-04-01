#lang racket/base

(provide (all-defined-out))

(require racket/list
         racket/contract/base
         rackjure/threading
         )

(define (flatten* . args)
  (flatten args))

(define (hash-remove* hsh keys)
  (for/fold ([hsh hsh]) ([k (in-list keys)])
    (hash-remove hsh k)))

(define (keyword->symbol kw)
  (~> kw keyword->string string->symbol))

(define (symbol->keyword sym)
  (~> sym symbol->string string->keyword))

(define (product nums)
  (apply * nums))

(define (ihasheq/c k v)
  (flat-named-contract
   `(ihasheq/c ,(contract-name k) ,(contract-name v))
   (and/c hash? hash-eq? (hash/c k v #:immutable #t))))

(define (member? x lst)
  (and (member x lst) #t))

