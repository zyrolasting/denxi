#lang racket/base

; Define a data type representing a user's possibly vague and/or
; ambiguous reference to a package. Detect and prefer any values
; that are neither.

(require "contract.rkt")

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
          [zcpkg-info->dependency (-> zcpkg-info? dependency?)]
          [dependency-match? (-> dependency-variant? dependency-variant? boolean?)]))


(require racket/match
         racket/exn
         racket/format
         racket/function
         "service/endpoint.rkt"
         "string.rkt"
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
             (or/c revision-number-string? revision-number?)
             boolean?
             (or/c revision-number-string? revision-number?))
   v))

; Recognize the many sources of a dependency declarations.
(define (dependency-variant? v)
  ((disjoin string?
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
       (equal? (normalize-revision-number (dependency-revision-min d))
               (normalize-revision-number (dependency-revision-max d)))))

(define (normalize-revision-number n)
  (if (revision-number-string? n)
      (string->number n)
      n))

(define (dependency-revision-min/for-inclusive-checks d)
  (add1-if (dependency-revision-min-exclusive? d)
           (normalize-revision-number (dependency-revision-min d))))

(define (dependency-revision-max/for-inclusive-checks d)
  (sub1-if (dependency-revision-max-exclusive? d)
           (normalize-revision-number (dependency-revision-max d))))

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
        [(zcpkg-info? v) (zcpkg-info->dependency v)]))

(define (dependency-string? s)
  (with-handlers ([(const #t) (const #f)])
    (and (string->dependency s) #t)))

(define (string->dependency s)
  (match (string-split s ":")
    [(list (? name-string? provider-name)
           (? name-string? package-name))
     (dependency provider-name
                 package-name
                 "draft"
                 #f "newest"
                 #f "newest")]

    [(list (? name-string? provider-name)
           (? name-string? package-name)
           (? name-string? edition-name))
     (dependency provider-name
                 package-name
                 edition-name
                 #f "newest"
                 #f "newest")]

    [(list (? name-string? provider-name)
           (? name-string? package-name)
           (? name-string? edition-name)
           (? revision-string? revision))
     (dependency provider-name
                 package-name
                 edition-name
                 #f revision
                 #f revision)]

    [(list (? name-string? provider-name)
           (? name-string? package-name)
           (? name-string? edition-name)
           (? revision-string? min-revision)
           (? revision-string? max-revision))
     (dependency provider-name
                 package-name
                 edition-name
                 #f min-revision
                 #f max-revision)]

    [(list (? name-string? provider-name)
           (? name-string? package-name)
           (? name-string? edition-name)
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
  (string-join
   (list (dependency-provider-name d)
         (dependency-package-name d)
         (dependency-edition-name d)
         (bool->exclusive-flag (dependency-revision-min-exclusive? d))
         (dependency-revision-min d)
         (bool->exclusive-flag (dependency-revision-max-exclusive? d))
         (dependency-revision-max d))
   ":"))

(define (exclusive-flag->bool flag name)
  (match flag
    ["i" #f]
    ["e" #t]
    [_ (raise-argument-error 'string->dependency
                             (format "\"i\" or \"e\" for ~a" name)
                             flag)]))

(define (bool->exclusive-flag expr)
  (if expr "e" "i"))


(module+ test
  (require rackunit)

  (test-pred "Detect well-formed dependencies"
             well-formed-dependency?
             (dependency "provider" "package" "edition" #t "0" #t "0"))

  (define ill-formed
    (list (dependency "" "package" "edition" #t "0" #t "0")
          (dependency "provider" "" "edition" #t "0" #t "0")
          (dependency "provider" "edition" "" #t "0" #t "0")
          (dependency "provider" "package" "edition" "#t" "0" #t "0")
          (dependency "provider" "package" "edition" #t #f #t "0")
          (dependency "provider" "package" "edition" #t "0" "#t" "0")
          (dependency "provider" "package" "edition" #t "0" #t #f)
          (dependency #f #f #f #f #f #f #f)))

  (for ([example (in-list ill-formed)])
    (test-false (format "Detect ill-formed dependency: ~s" example)
                (well-formed-dependency? example)))

  (test-true "Detect concrete dependency"
             (concrete-dependency? (dependency "provider" "package" "edition" #t "0" #t "10")))
  (test-false "Detect abstract dependency"
              (concrete-dependency? (dependency "provider" "package" "edition" #t "initial" #t "10")))

  (test-true "Exact dependency: One possible version"
             (exact-dependency? (dependency "p" "p" "p" #f "0" #f "0")))
  (test-false "Prohibit exact dependency from having exclusive lower bound"
              (exact-dependency? (dependency "p" "p" "p" #t "0" #f "0")))
  (test-false "Prohibit exact dependency from having exclusive upper bound"
              (exact-dependency? (dependency "p" "p" "p" #f "0" #t "0")))
  (test-false "Prohibit exact dependency from varying on version"
              (exact-dependency? (dependency "p" "p" "p" #f "0" #f "1")))

  (test-case "Convert between dependency instances and their representations"
    (define target (dependency "joe" "pkg" "edition" #f "8" #f "8"))
    (define str-repr "joe:pkg:edition:i:8:i:8")
    (define info-repr (zcpkg-info "joe" "pkg" "edition" "8" #f #f #f #f #f #f))
    (check-equal? target (string->dependency str-repr))
    (check-equal? target (coerce-dependency str-repr))
    (check-equal? target (zcpkg-info->dependency info-repr))
    (check-equal? target (coerce-dependency info-repr))
    (check-equal? str-repr (dependency->string target)))


  (test-true "Detect equal dependency identities"
             (dependency-identity=? (dependency "a" "b" "c" #f #f #f #f)
                                    (dependency "a" "b" "c" #f #f #f #f)))

  (test-false "Detect differing provider names"
              (dependency-identity=? (dependency "a" "b" "c" #f #f #f #f)
                                     (dependency " " "b" "c" #f #f #f #f)))

  (test-false "Detect differing package names"
              (dependency-identity=? (dependency "a" "b" "c" #f #f #f #f)
                                     (dependency "a" " " "c" #f #f #f #f)))

  (test-false "Detect differing edition names"
              (dependency-identity=? (dependency "a" "b" "c" #f #f #f #f)
                                     (dependency "a" "b" " " #f #f #f #f)))


  (test-case "Use strings to produce exact and inexact dependencies"
    (define (check-conversion dep str)
      (check-equal? dep (string->dependency str)))

    (check-conversion (dependency "joe" "pkg" "draft" #f "newest" #f "newest")
                      "joe:pkg")

    (check-conversion (dependency "joe" "pkg" "edition" #f "newest" #f "newest")
                      "joe:pkg:edition")

    (check-conversion (dependency "joe" "pkg" "edition" #f "initial" #f "initial")
                      "joe:pkg:edition:initial")

    (check-conversion (dependency "joe" "pkg" "edition" #f "min" #f "max")
                      "joe:pkg:edition:min:max")

    (check-conversion (dependency "joe" "pkg" "edition" #f "beta" #t "prod")
                      "joe:pkg:edition:i:beta:e:prod"))

  (test-pred "Use zcpkg-info instances to produce exact dependencies"
             exact-dependency?
             (zcpkg-info->dependency (zcpkg-info "joe" "pkg" "edition" "8" #f #f #f #f #f #f)))


  (test-true "Match a dependency using a revision range"
             (dependency-match? "joe:pkg:draft:0:100" "joe:pkg:draft:0:0"))

  (test-true "Match a dependency using a revision range, even with string+number mixes"
             (dependency-match? (dependency "joe" "pkg" "draft" #f "0" #f 100)
                                (dependency "joe" "pkg" "draft" #f 0 #f "0")))

  (test-true "Match a dependency exactly"
             (dependency-match? "joe:pkg:draft:2:2" "joe:pkg:draft:2:2"))

  (test-true "Match a dependency that risks an off-by-one error (lower bound)"
             (dependency-match? "joe:pkg:draft:e:1:i:3"
                                "joe:pkg:draft:2:2"))

  (test-true "Match a dependency that risks an off-by-one error (upper bound)"
             (dependency-match? "joe:pkg:draft:i:1:e:3"
                                "joe:pkg:draft:2:2"))

  (test-false "Do not match a dependency that differs in provider name"
              (dependency-match? "joe:pkg:draft:i:1:e:3"
                                 "je:pkg:draft:2:2"))

  (test-false "Do not match a dependency that differs in package name"
              (dependency-match? "joe:pkg:draft:i:1:e:3"
                                 "joe:pg:draft:2:2"))

  (test-false "Do not match a dependency that differs in edition name"
              (dependency-match? "joe:pkg:draft:i:1:e:3"
                                 "joe:pkg:drft:2:2"))

  (test-exn "Raise a contract error if comparing two inexact dependencies"
            #rx"expected: An exact dependency"
            (λ () (dependency-match? "joe:pkg:draft:i:1:e:3"
                                     "joe:pkg")))

  (test-exn "Raise a contract error if matching against an ill-formed dependency"
            #rx"expected: A concrete dependency"
            (λ () (dependency-match? (dependency #f #f #f #f #f #f #f)
                                     "joe:pkg:draft:1:1")))

  (test-exn "Raise a contract error if matching against an abstract dependency"
            #rx"expected: A concrete dependency"
            (λ () (dependency-match? "joe:pkg"
                                     "joe:pkg:draft:1:1")))

  (test-exn "Raise a special error if matching against an invalid interval"
            exn:fail:zcpkg:invalid-revision-interval?
            (λ () (dependency-match? "joe:pkg:draft:10:1"
                                     "joe:pkg:draft:1"))))
