#lang info

(define collection 'multi)

(define deps '("base"
               "math-lib"
               "unstable-list-lib"
               "typed-racket-lib"
               "sweet-exp"
               "lens"
               "generic-bind"
               "rackjure"
               "kw-utils"
               "match-string"
               "my-cond"
               "git://github.com/AlexKnauth/my-plai"
               ))

(define build-deps '("rackunit-lib"
                     "git://github.com/AlexKnauth/testing-utils"
                     ))

