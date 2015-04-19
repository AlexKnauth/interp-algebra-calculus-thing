#lang sweet-exp racket/base

provide scope? scope-set?

require racket/contract/base
        racket/set

define scope? symbol?

define scope-set?
  flat-named-contract 'scope-set?
    set/c(scope? #:kind 'immutable #:cmp 'eq)

