#lang racket/base

(provide (all-defined-out))

(require racket/match
         racket/contract/base
         racket/contract/region
         racket/dict
         racket/set
         syntax/srcloc
         "scope-set.rkt"
         "syntax.rkt"
         "ExprC.rkt"
         "../../stuff.rkt"
         )

;; an (Id-Tableof A) is an (id-table (Hasheqof Symbol (Hashequalof Scope-Set A)))
(struct id-table (hsh)
  #:transparent
  #:guard
  (lambda (hsh _)
    (unless (id-table-hsh? hsh)
      (error 'id-table "expected (Hasheqof Symbol (Hashequalof Scope-Set Any)), given: ~v" hsh))
    (values hsh))
  #:methods gen:dict
  [(define (dict-ref dict key [fail (λ () (error 'id-table-bad))])
     (id-table-ref dict key fail))]
  )

;; an Id-Set is an (id-set (Hasheqof Symbol (Setequalof Scope-Set)))
(struct id-set (hsh)
  #:transparent
  #:guard
  (lambda (hsh _)
    (unless (id-set-hsh? hsh)
      (error 'id-set "expected (Hasheqof Symbol (Setof Scope-Set)), given: ~v" hsh))
    (values hsh))
  #:methods gen:set
  [(define (set-member? st id)
     (id-set-member? st id))
   (define (set-add st id)
     (id-set-add st id))
   ;(define (set-remove st i)
   ;  ....)
   ]
  )


(define (id-table-hshof A)
  (flat-named-contract
   `(id-table-hshof ,(contract-name A))
   (ihasheq/c symbol? (ihashequal/c scope-set? A))))

(define id-table-hsh? (id-table-hshof any/c))

(define (id-tableof A)
  (flat-named-contract
   `(id-tableof ,(contract-name A))
   (λ (x)
     (and (id-table? x)
          ((id-table-hshof A) (id-table-hsh x))))))

(define empty-id-table (id-table (hasheq)))

(define id-set-hsh?
  (flat-named-contract
   'id-set-hsh?
   (ihasheq/c symbol? (set/c scope-set? #:kind 'immutable #:cmp 'equal))))

(define empty-id-set (id-set (hasheq)))

(define/contract (id-table-ref tbl id [fail-proc (λ () #f)])
  [[id-table? idC?] [(-> any)] . ->* . any]
  (match-define (id-table hsh) tbl)
  (match-define (idC sym id-scopes srcloc) (id->idC id))
  (define scopes->val (hash-ref hsh sym (λ () (hash))))
  (define-values (v s)
    (for/fold ([v #f] [s (seteq)])
              ([(scopes val) (in-hash scopes->val)]
               #:when (scopes . subset? . id-scopes))
      (cond [(s . subset? . scopes)
             (values val scopes)]
            [(scopes . subset? . s)
             (values v s)]
            [else
             (raise-syntax-error 'id-table-ref "ambiguous binding" (idC->rkt:id id))])))
  (cond [v v]
        [else (fail-proc)]))

(define (id-set-member? st id)
  (match-define (id-set hsh) st)
  (match-define (idC sym id-scopes srcloc) (id->idC id))
  (define scopess
    (hash-ref hsh sym (λ () (set))))
  (for/or ([scopes (in-set scopess)])
    (scopes . subset? . id-scopes)))

(define (id-set-add st id)
  (match-define (id-set hsh) st)
  (id-set (id-set-hsh-add hsh id)))

(define (id-set-hsh-add hsh id)
  (match-define (idC sym id-scopes srcloc) (id->idC id))
  (hash-update hsh sym
    (λ (scopess)
      (set-add scopess id-scopes))
    (λ ()
      (set))))

(define (id-list->id-set id-lst)
  (id-set
   (for/fold ([hsh (hasheq)]) ([id (in-list id-lst)])
     (id-set-hsh-add hsh id))))



(define/contract (idC->rkt:id id #:ctxt [ctxt #f] #:props [props #f])
  [[idC?] [#:ctxt (or/c syntax? #f) #:props (or/c syntax? #f)] . ->* . identifier?]
  (match-define (idC sym scopes srcloc) (id->idC id))
  (datum->syntax ctxt sym (build-source-location-list srcloc) props))

