#lang at-exp sweet-exp racket

provide all-defined-out()

require racket/match
        racket/set
        syntax/srcloc
        syntax/parse/define
        my-cond/iffy
        prefix-in rkt: racket/base
        "scope-set.rkt"
        "syntax-object.rkt"
        for-syntax racket/base
                   syntax/parse

define syntax?(x)
  Syntax?(x)

define syntax-e?(x)
  syntax-object-e?(x)

define syntax-e(x)
  syntax-object-e(x)

define syntax-srcloc(x)
  syntax-object-srcloc(x)

define syntax->datum(x)
  syntax-object->datum(x)

define ->srcloc(srcloc)
  my-cond
    if syntax-object?(srcloc)
      syntax-object-srcloc(srcloc)
    else-if source-location?(srcloc)
      build-source-location(srcloc)
    else
      error('->srcloc "expected (or/c syntax? source-location?), given: ~v" srcloc)

define ->scopes(scopes)
  my-cond
    if syntax-object?(scopes)
      syntax-object-scopes(scopes)
    else-if scope-set?(scopes)
      scopes
    else-if false?(scopes)
      seteq()
    else
      error('->scopes "expected (or/c syntax? scope-set?), given: ~v" scopes)

define datum->syntax(e #:srcloc [srcloc #f] #:scopes [scopes #f])
  my-cond
    if syntax?(e)
      e
    else-if syntax-e?(e)
      syntax-object
        e
        ->srcloc(srcloc)
        ->scopes(scopes)
    else error('datum->syntax "given: ~v" e)

define rkt:syntax->syntax(stx)
  my-cond
    if rkt:syntax?(stx)
      rkt:stx->syntax(stx)
    else
      error('rkt:syntax->syntax "expected rkt:syntax?, given: ~v" stx)

define srcloc-or
  case-lambda
    [()
      ->srcloc(#f)]
    (a)
      ->srcloc(a)
    (a . bs)
      define a* ->srcloc(a)
      my-cond
        if source-location-known?(a*)
          a*
        else
          apply srcloc-or bs

define rkt:stx->syntax(stx #:srcloc [srcloc #f])
  match stx
    syntax-object(e srcloc* scopes)
      syntax-object(e srcloc-or(srcloc* srcloc) scopes)
    ?[rkt:syntax? stx]
      rkt:stx->syntax(rkt:syntax-e(stx) #:srcloc srcloc-or(stx srcloc))
    ?[symbol? sym]
      datum->syntax sym #:srcloc srcloc
    ?[empty?]
      datum->syntax '() #:srcloc srcloc
    cons(car cdr)
      datum->syntax cons(rkt:stx->syntax(car) rkt:stx->syntax(cdr)) #:srcloc srcloc
    ?[number? n]
      datum->syntax n #:srcloc srcloc

define-simple-macro quote-syntax(stx)
  rkt:syntax->syntax rkt:quote-syntax(stx)

define id->rkt:id(id)
  match-define syntax-object(sym srcloc scopes) id
  datum->syntax #f sym build-source-location-list(srcloc)

define stx->rkt:stx(stx)
  match stx
    ?[rkt:syntax? stx]
      stx
    syntax-object(e srcloc scopes)
      rkt:datum->syntax #f stx->rkt:stx(e) build-source-location-list(srcloc)
    ?[symbol? sym]
      sym
    ?[empty?]
      '()
    cons(car cdr)
      cons(stx->rkt:stx(car) stx->rkt:stx(cdr))
    ?[number? n]
      n



