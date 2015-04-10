#lang racket/base

(require syntax/parse/define
         "../ops.rkt"
         (for-syntax racket/base
                     syntax/parse
                     ))

(define-simple-macro (∑ [x:id seq:expr] body:expr)
  (apply + (for/list ([x seq]) body)))

