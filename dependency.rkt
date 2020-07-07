#lang racket/base

; Define a data type representing a user's possibly vague and/or
; ambiguous reference to a package. Detect and prefer any values
; that are neither.

(require idiocket/contract)

(provide (struct-out dependency)
         (contract-out
          [well-formed-dependency? predicate/c]
          [concrete-dependency? predicate/c]
          [exact-dependency? predicate/c]
          [dependency-string? predicate/c]
          [dependency-variant? predicate/c]
          [dependency-identity=? (-> dependency-variant? dependency-variant? boolean?)]
          [coerce-dependency (-> dependency-variant? dependency?)]
          [dependency->string (-> dependency? string?)]
          [string->dependency (-> string? dependency?)]
          [dependency->url (-> dependency? url?)]
          [url->dependency (-> url? dependency?)]
          [zcpkg-info->dependency (-> zcpkg-info? dependency?)]
          [dependency-match? (-> dependency-variant? dependency-variant? boolean?)]))


(require idiocket/match
         idiocket/exn
         idiocket/format
         idiocket/function
         "service/endpoint.rkt"
         "string.rkt"
         "url.rkt"
         "zcpkg-info.rkt"
         "revision.rkt")

(struct dependency
  (provider-name
   package-name
   edition-name
   revision-min-exclusive?
   revision-min
   revision-max-exclusive?
   revision-max)
  #:transparent)

; Define "Well-formed" to mean that a dependency structure
; has correct value types for referencing some package.
(define (well-formed-dependency? v)
  (passes-invariant-assertion?
   (struct/c dependency
             name-string?
             name-string?
             name-string?
             boolean?
             revision-string?
             boolean?
             revision-string?)
   v))

; Define a "concrete" dependency as a well-formed dependency
; with a revision number range for some package.
(define (concrete-dependency? v)
  (passes-invariant-assertion?
   (struct/c dependency
             name-string?
             name-string?
             name-string?
             boolean?
             revision-number-string?
             boolean?
             revision-number-string?)
   v))

; Recognize the many sources of a dependency declarations.
(define (dependency-variant? v)
  ((disjoin url?
            string?
            dependency?
            zcpkg-info?) v))

; Define an exact dependency as a concrete dependency where the
; revision number range has exactly one element.  Assuming a
; provider's identity is not compromised, an exact dependency is
; globally unique.
(define (exact-dependency? d)
  (and (concrete-dependency? d)
       (not (dependency-revision-min-exclusive? d))
       (not (dependency-revision-max-exclusive? d))
       (equal? (dependency-revision-min d)
               (dependency-revision-max d))))

(define (dependency-revision-min/for-inclusive-checks d)
  (add1-if (dependency-revision-min-exclusive? d)
           (string->number (dependency-revision-min d))))

(define (dependency-revision-max/for-inclusive-checks d)
  (sub1-if (dependency-revision-max-exclusive? d)
           (string->number (dependency-revision-max d))))

; Check if a well-formed dependency encompasses an exact dependency.
(define (dependency-match? to-match/variant exact-dep/variant)
  (define to-match (coerce-dependency to-match/variant))
  (define exact-dep (coerce-dependency exact-dep/variant))

  (unless (concrete-dependency? to-match)
    (raise-argument-error 'dependency-match?
                          "A concrete dependency declaration"
                          0
                          to-match/variant
                          exact-dep/variant))

  (unless (exact-dependency? exact-dep)
    (raise-argument-error 'dependency-match?
                          "An exact dependency declaration"
                          1
                          to-match/variant
                          exact-dep/variant))

  (define-values (l1 h1 l2 h2)
    (values (dependency-revision-min/for-inclusive-checks exact-dep)
            (dependency-revision-max/for-inclusive-checks exact-dep)
            (dependency-revision-min/for-inclusive-checks to-match)
            (dependency-revision-max/for-inclusive-checks to-match)))

  (assert-valid-revision-range l1 h1 "haystack dependency")
  (assert-valid-revision-range l2 h2 "needle dependency")

  (and (dependency-identity=? exact-dep to-match)
       (revision-range-subset? l1 h1 l2 h2)))

(define (dependency-identity=? a b)
  (define x (coerce-dependency a))
  (define y (coerce-dependency b))
  (andmap (λ (p) (equal? (p x) (p y)))
          (list dependency-provider-name
                dependency-package-name
                dependency-edition-name)))

(define (coerce-dependency v)
  (cond [(dependency? v) v]
        [(string? v) (string->dependency v)]
        [(url? v) (url->dependency v)]
        [(zcpkg-info? v) (zcpkg-info->dependency v)]))

(define (dependency-string? s)
  (fail-as #f (string->dependency s)))

(define (string->dependency s)
  (url->dependency (string->url s)))

(define (url->dependency u)
  (match-define
    (list (path/param provider-name _)
          (path/param package-name version-info))
    (url-path u))

  (match version-info
    [(list)
     (dependency provider-name
                 package-name
                 "draft"
                 #f "newest"
                 #f "newest")]

    [(list (? name-string? edition-name))
     (dependency provider-name
                 package-name
                 edition-name
                 #f "newest"
                 #f "newest")]

    [(list (? name-string? edition-name)
           (? revision-string? revision))
     (dependency provider-name
                 package-name
                 edition-name
                 #f revision
                 #f revision)]

    [(list (? name-string? edition-name)
           (? revision-string? min-revision)
           (? revision-string? max-revision))
     (dependency provider-name
                 package-name
                 edition-name
                 #f min-revision
                 #f max-revision)]

    [(list (? name-string? edition-name)
           min-flag
           (? revision-string? min-revision)
           max-flag
           (? revision-string? max-revision))
     (dependency provider-name
                 package-name
                 edition-name
                 (exclusive-flag->bool min-flag "revision minimum flag")
                 min-revision
                 (exclusive-flag->bool max-flag "revision maximum flag")
                 max-revision)]))

(define (zcpkg-info->dependency info)
  (dependency (zcpkg-info-provider-name info)
              (zcpkg-info-package-name info)
              (zcpkg-info-edition-name info)
              #f
              (zcpkg-info-revision-number info)
              #f
              (zcpkg-info-revision-number info)))

(define (dependency->string d)
  (url->string (url #f #f #f #f #f (url-path (dependency->url d)) null #f)))

(define (dependency->url d)
  (make-endpoint
   [path
    (list (path/param (dependency-provider-name d) null)
          (path/param (dependency-package-name d)
                      (list (dependency-edition-name d)
                            (bool->exclusive-flag (dependency-revision-min-exclusive? d))
                            (dependency-revision-min d)
                            (bool->exclusive-flag (dependency-revision-max-exclusive? d))
                            (dependency-revision-max d))))]))


(define (exclusive-flag->bool flag name)
  (match flag
    ["i" #f]
    ["e" #t]
    [_ (raise-argument-error 'url->dependency
                             (format "\"i\" or \"e\" for ~a" name)
                             flag)]))

(define (bool->exclusive-flag expr)
  (if expr "e" "i"))
