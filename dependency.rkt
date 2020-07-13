#lang racket/base

; Define a data type used to match against zcpkg-info instances.  This
; data type must represent a possibly ambiguous reference to a
; package. Detect and prefer any values that are neither.

(require "contract.rkt")

(provide (struct-out dependency)
         (contract-out
          [CONVENTIONAL_NEWEST_REVISION_NAME string?]
          [revision-string? predicate/c]
          [revision-number? predicate/c]
          [revision-number-string? predicate/c]
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
          [search-zcpkg-infos (-> dependency? (sequence/c zcpkg-info?) zcpkg-info?)]
          [dependency-match? (-> dependency-variant? dependency-variant? boolean?)]))


(require racket/exn
         racket/format
         racket/function
         racket/match
         racket/generator
         racket/match
         racket/sequence
         "service/endpoint.rkt"
         "string.rkt"
         "zcpkg-info.rkt")

; Define an exception capturing a reversed revision range like [8, 1].
(struct exn:fail:zcpkg:invalid-revision-interval exn:fail (lo hi))

(define CONVENTIONAL_NEWEST_REVISION_NAME "newest")

(define revision-number-pattern-string "\\d+")

(define revision-number-string?
  (make-rx-predicate revision-number-pattern-string))

(define revision-number?
  exact-nonnegative-integer?)

(define revision-string?
  (or/c revision-number-string?
        name-string?))

(define (coerce-revision-number n)
  (if (revision-number-string? n)
      (string->number n)
      n))

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
       (equal? (coerce-revision-number (dependency-revision-min d))
               (coerce-revision-number (dependency-revision-max d)))))

; Define a procedure to search package metadata.
(define (search-zcpkg-infos dep-variant infos)
  (define dep (coerce-dependency dep-variant))
  (define revisions (filter-revisions dep infos))
  (define sorted (sort-revisions (sequence->list revisions)))
  (define min-revision-number (find-revision-number (dependency-revision-min dep) sorted))
  (define max-revision-number (find-revision-number (dependency-revision-max dep) sorted))
  (define-values (min-adjusted max-adjusted)
    (get-dependency-revision-range dep
                                   #:lo min-revision-number
                                   #:hi max-revision-number))

  (assert-valid-revision-range min-adjusted max-adjusted (dependency->string dep))
  (sequence-filter (λ (info)
                     (in? (zcpkg-info-revision-number info)
                          min-adjusted
                          max-adjusted))
                   sorted))


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

  (define-values (l1 h1)
    (get-dependency-revision-range exact-dep))
  (define-values (l2 h2)
    (get-dependency-revision-range to-match))

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

(define (filter-revisions dep infos)
  (sequence-filter (λ (info)
                     (zcpkg-non-revision-identity=?
                      info
                      (dependency-provider-name dep)
                      (dependency-package-name dep)
                      (dependency-edition-name dep)))
                   infos))

(define (sort-revisions infos)
  (sort infos (λ (a b) (> (zcpkg-compare-versions a b) 0))))

(define (in? n l h)
  (and (>= n l)
       (<= n h)))

(define (revision-range-subset? l1 h1 l2 h2)
  (and (in? l1 l2 h2)
       (in? h1 l2 h2)))

(define (get-dependency-revision-range d
                                       #:lo [lo (dependency-revision-min d)]
                                       #:hi [hi (dependency-revision-max d)])
  (define (add1-if do-it? low-num)
    (if do-it? (add1 low-num) low-num))
  (define (sub1-if do-it? hi-num)
    (if do-it? (sub1 hi-num) hi-num))
  (values (add1-if (dependency-revision-min-exclusive? d)
                   (coerce-revision-number lo))
          (sub1-if (dependency-revision-max-exclusive? d)
                   (coerce-revision-number hi))))

; Assumes infos are sorted from newest to oldest.
(define (find-revision-number variant infos)
  (cond [(revision-number? variant) variant]
        [(revision-number-string? variant)
         (string->number variant)]
        [(equal? variant CONVENTIONAL_NEWEST_REVISION_NAME)
         (zcpkg-info-revision-number (sequence-ref infos 0))]
        [(string? variant)
         (for/or ([info infos])
           (and (member variant (zcpkg-info-revision-names info))
                (zcpkg-info-revision-number info)))]
        [else #f]))

(define (assert-valid-revision-range lo hi [named-interval #f])
  (when (< hi lo)
    (raise
     (exn:fail:zcpkg:invalid-revision-interval
      (if named-interval
          (format (string-append
                   "~v makes invalid interval [~v, ~v]. "
                   "If the names are correct, then are they reversed?")
                  named-interval lo hi)
          (format "Cannot use revision interval [~v, ~v]: The upper bound is less than the lower bound."
                  lo hi))
      (current-continuation-marks)
      lo
      hi))))


(module+ test
  (require rackunit)

  (define ill-formed
    (list (dependency "" "package" "edition" #t "0" #t "0")
          (dependency "provider" "" "edition" #t "0" #t "0")
          (dependency "provider" "edition" "" #t "0" #t "0")
          (dependency "provider" "package" "edition" "#t" "0" #t "0")
          (dependency "provider" "package" "edition" #t #f #t "0")
          (dependency "provider" "package" "edition" #t "0" "#t" "0")
          (dependency "provider" "package" "edition" #t "0" #t #f)
          (dependency #f #f #f #f #f #f #f)))


  (define (test-true* p seq)
    (for ([args (in-values-sequence seq)])
      (test-true (format "Expect: (eq? #t (~a ~a))"
                         (object-name p)
                         (string-join (map ~v args)
                                      " "))
                 (apply p args))))

  (test-pred "Detect well-formed dependencies"
             well-formed-dependency?
             (dependency "provider" "package" "edition" #t "0" #t "0"))

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
                                     "joe:pkg:draft:1")))


  (test-true "Accept (large) positive integers"
             (revision-number-string? "87897679687236872363278692984"))

  (test-true "Accept \"0\""
             (revision-number-string? "0"))

  (test-false "Reject negative numbers"
              (revision-number-string? "-1"))

  (test-false "Reject floating-point numbers"
              (revision-number-string? "1.224"))

  (test-false "Reject expressions"
              (revision-number-string? "1/2"))

  (test-false "Reject alphanumeric"
              (revision-number-string? "70O"))

  (test-case "Metadata search"
    (define (zpi pr pk ed num names)
      (zcpkg-info pr pk ed num names #f null #"" #"" 0))

    (define infos
      (list (zpi "alice" "wonderland" "mad" 0 '("initial"))
            (zpi "bob"   "rob"        "sob" 2 null)
            (zpi "bob"   "rob"        "sob" 0 null)
            (zpi "alice" "wonderland" "mad" 3 null)
            (zpi "alice" "wonderland" "bad" 0 '("adventure"))
            (zpi "alice" "wonderland" "mad" 1 null)
            (zpi "bob"   "rob"        "sob" 1 null)
            (zpi "alone" "only"       "one" 0 null)
            (zpi "alice" "wonderland" "bad" 1 null)
            (zpi "alice" "wonderland" "mad" 2 '("after-patch" "new-beta"))
            (zpi "alice" "wonderland" "mad" 5 '("latest"))
            (zpi "alice" "wonderland" "mad" 4 '("retracted"))))

    (define mad-wonderland/unsorted
      (filter-revisions (coerce-dependency "alice:wonderland:mad") infos))
    (define mad-wonderland
      (sort-revisions (sequence->list mad-wonderland/unsorted)))

    (define mad-wonderland/expected-sort
      (list (zpi "alice" "wonderland" "mad" 5 '("latest"))
            (zpi "alice" "wonderland" "mad" 4 '("retracted"))
            (zpi "alice" "wonderland" "mad" 3 null)
            (zpi "alice" "wonderland" "mad" 2 '("after-patch" "new-beta"))
            (zpi "alice" "wonderland" "mad" 1 null)
            (zpi "alice" "wonderland" "mad" 0 '("initial"))))


    (test-equal? "Can filter and sort info by revision number"
                 mad-wonderland
                 mad-wonderland/expected-sort)

    (test-eq? "Can look up a revision by name"
              (find-revision-number "initial" mad-wonderland)
              0)

    (test-eq? "A revision can have multiple names"
              (find-revision-number "after-patch" mad-wonderland)
              (find-revision-number "new-beta" mad-wonderland))

    (test-eq? (format "~s is implicit" CONVENTIONAL_NEWEST_REVISION_NAME)
              (find-revision-number CONVENTIONAL_NEWEST_REVISION_NAME mad-wonderland)
              (find-revision-number "latest" mad-wonderland))

    (test-equal? "Search: alice:wonderland:mad:0:newest"
                 (sequence->list (search-zcpkg-infos "alice:wonderland:mad:0:newest" infos))
                 mad-wonderland/expected-sort)

    (test-equal? "Search: alice:wonderland:bad:0:1"
                 (sequence->list (search-zcpkg-infos "alice:wonderland:bad:0:1" infos))
                 (list (zpi "alice" "wonderland" "bad" 1 null)
                       (zpi "alice" "wonderland" "bad" 0 '("adventure"))))

    (test-equal? "Search: bob:rob:sob:i:0:e:1"
                 (sequence->list (search-zcpkg-infos "bob:rob:sob:i:0:e:1" infos))
                 (list (zpi "bob" "rob" "sob" 0 null)))

    (test-exn "Catch impossible revision range: [2, 0]"
              (λ (e)
                (and (exn:fail:zcpkg:invalid-revision-interval? e)
                     (eq? 2 (exn:fail:zcpkg:invalid-revision-interval-lo e))
                     (eq? 0 (exn:fail:zcpkg:invalid-revision-interval-hi e))))
              (λ () (search-zcpkg-infos "bob:rob:sob:e:1:e:1" infos)))

    (test-exn "[newest, 0] is impossible when there is more than one revision"
              exn:fail:zcpkg:invalid-revision-interval?
              (λ () (search-zcpkg-infos "bob:rob:sob:i:newest:i:0" infos)))

    (test-equal? "[newest, 0] makes sense when there is only one revision"
                 (sequence-ref (search-zcpkg-infos "alone:only:one:i:newest:i:0" infos) 0)
                 (zpi "alone" "only" "one" 0 null))))
