#lang racket/base

(provide (all-defined-out))

(require racket/match
         racket/contract/base
         syntax/srcloc
         my-plai/my-type-case
         "scope-set.rkt"
         "syntax-object.rkt"
         )

(define-type ExprC
  [valC [val any/c] [srcloc source-location?]]
  [idC [sym symbol?] [scopes scope-set?] [srcloc source-location?]]
  [letC [vars (listof (list/c idC? ExprC?))] [body ExprC?] [srcloc source-location?]]
  [fnC [ids (listof idC?)] [body ExprC?] [srcloc source-location?]]
  [appC [f ExprC?] [args (listof (or/c ExprC? (list/c keyword? ExprC?)))] [srcloc source-location?]]
  [ifC [a ExprC?] [b ExprC?] [c ExprC?] [srcloc source-location?]])
;; Because of the substitute function, identifiers in the sense of idC cannot be mutable.
;; However, there is nothing preventing mutable data structures such as mutable boxes from being used
;; in a valC, and nothing preventing an identifier macro from expanding to a reference to a mutable
;; box, so things that look like variables are still possible in the surface syntax.

(define (id-or-idC? x)
  (or (identifier? x) (idC? x)))

(define (id->idC id)
  (match id
    [(idC _ _ _) id]
    [(syntax-object (? symbol? sym) srcloc scopes)
     (idC sym scopes srcloc)]))

(define (idC->id id)
  (match id
    [(syntax-object (? symbol?) _ _) id]
    [(idC sym scopes srcloc)
     (syntax-object sym srcloc scopes)]))

