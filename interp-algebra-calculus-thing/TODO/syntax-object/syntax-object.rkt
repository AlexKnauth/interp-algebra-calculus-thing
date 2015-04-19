#lang sweet-exp racket/base

provide all-defined-out()

require racket/match
        racket/contract/base
        racket/list
        generic-bind/as-rkt-names
        my-plai/my-type-case
        "scope-set.rkt"

define-type Syntax
  syntax-object
    e
      syntax-object-e?
    srcloc
      srcloc?
    scopes
      scope-set?
    ;props
    ;  syntax-properties?

define-type ExprS-struct
  letS
    vars (listof (list/c identifier? Syntax?))
    body Syntax?
  fnS
    syms (listof identifier?)
    body Syntax?
  appS
    f Syntax?
    args (listof Syntax?)
  ifS
    a Syntax?
    b Syntax?
    c Syntax?
  letstxS
    vars (listof (list/c identifier? Syntax?))
    body Syntax?

;define syntax-properties?
;  flat-named-contract 'syntax-properties?
;    {hash? and hash-eq? and
;           hash/c(symbol? any/c #:immutable #t)}

define syntax-object-e?(x)
  match x
    ?[ExprS-struct?] #t
    ?[symbol?] #t
    ?[empty?] #t
    cons(?[Syntax?] {?[Syntax?] or ?[empty?] or ?[pair*syntax-object-e?]}) #t
    ?[number?] #t

define pair*syntax-object-e?(x)
  {pair?(x) and syntax-object-e?(x)}

define syntax-object->datum(x)
  match x
    syntax-object(e _ _)
      syntax-object->datum(e)
    ?[ExprS-struct? s]
      ExprS-struct->datum/ExprS-struct(s)
    ?[symbol? sym] sym
    ?[empty? mt] mt
    cons(car cdr) cons(syntax-object->datum(car) syntax-object->datum(cdr))
    ?[number? n] n

define ExprS-struct->datum/ExprS-struct(e)
  my-type-case ExprS-struct e
    group
      letS vars body
      `(let ,(for/list ([bind (in-list vars)])
               (map syntax-object->datum bind))
         ,(syntax-object->datum body))
    group
      fnS ids body
      `(fn ,(map syntax-object->datum ids)
         ,(syntax-object->datum body))
    group
      appS f args
      `(#%app ,(syntax-object->datum f) . ,(map syntax-object->datum args))
    group
      ifS a b c
      `(if ,(syntax-object->datum a) ,(syntax-object->datum b) ,(syntax-object->datum c))
    group
      letstxS vars body
      `(let-syntax ,(for/list ([bind (in-list vars)])
                      (map syntax-object->datum bind))
         ,(syntax-object->datum body))

define identifier?(x)
  {syntax?(x) and symbol?(syntax-e(x))}

define keyword-stx?(x)
  {syntax?(x) and keyword?(syntax-e(x))}

