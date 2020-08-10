#lang racket/base

; Define a data type used to match against zcpkg-info instances.  This
; data type must represent a possibly ambiguous reference to a
; package. Detect and prefer any values that are neither.

(require "contract.rkt")

(provide (struct-out zcpkg-query)
         (struct-out exn:fail:zcpkg:invalid-revision-interval)
         (contract-out
          [CONVENTIONAL_NEWEST_REVISION_NAME string?]
          [revision-string? predicate/c]
          [revision-number? predicate/c]
          [revision-number-string? predicate/c]
          [well-formed-zcpkg-query? predicate/c]
          [concrete-zcpkg-query? predicate/c]
          [exact-zcpkg-query? predicate/c]
          [zcpkg-query-string? predicate/c]
          [zcpkg-query-variant? predicate/c]
          [zcpkg-query-identity=? (-> zcpkg-query-variant? zcpkg-query-variant? boolean?)]
          [coerce-zcpkg-query (-> zcpkg-query-variant? zcpkg-query?)]
          [zcpkg-query->string (-> zcpkg-query? string?)]
          [get-inclusive-revision-range
           (->* (any/c any/c revision-number? revision-number?)
                (#:named-interval (or/c #f string?))
                (values revision-number? revision-number?))]
          [string->zcpkg-query (-> string? zcpkg-query?)]
          [zcpkg-info->zcpkg-query (-> zcpkg-info? zcpkg-query?)]
          [search-zcpkg-infos (-> zcpkg-query-variant? (sequence/c zcpkg-info?) (sequence/c zcpkg-info?))]
          [zcpkg-query-match? (-> zcpkg-query-variant? zcpkg-query-variant? boolean?)]))


(require racket/exn
         racket/format
         racket/function
         racket/match
         racket/generator
         racket/match
         racket/sequence
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

(struct zcpkg-query
  (provider-name
   package-name
   edition-name
   revision-min-exclusive?
   revision-min
   revision-max-exclusive?
   revision-max)
  #:transparent)

; Define "Well-formed" to mean that a zcpkg-query structure
; has correct value types for referencing some package.
(define (well-formed-zcpkg-query? v)
  (passes-invariant-assertion?
   (struct/c zcpkg-query
             name-string?
             name-string?
             name-string?
             boolean?
             revision-string?
             boolean?
             revision-string?)
   v))

; Define a "concrete" zcpkg-query as a well-formed zcpkg-query
; with a revision number range for some package.
(define (concrete-zcpkg-query? v)
  (passes-invariant-assertion?
   (struct/c zcpkg-query
             name-string?
             name-string?
             name-string?
             boolean?
             (or/c revision-number-string? revision-number?)
             boolean?
             (or/c revision-number-string? revision-number?))
   v))

; Recognize the many sources of a zcpkg-query declarations.
(define (zcpkg-query-variant? v)
  ((disjoin string?
            zcpkg-query?
            zcpkg-info?) v))

; Define an exact zcpkg-query as a concrete zcpkg-query where the
; revision number range has exactly one element.  Assuming a
; provider's identity is not compromised, an exact zcpkg-query is
; globally unique.
(define (exact-zcpkg-query? d)
  (and (concrete-zcpkg-query? d)
       (not (zcpkg-query-revision-min-exclusive? d))
       (not (zcpkg-query-revision-max-exclusive? d))
       (equal? (coerce-revision-number (zcpkg-query-revision-min d))
               (coerce-revision-number (zcpkg-query-revision-max d)))))

; Define a procedure to search package metadata.
(define (search-zcpkg-infos dep-variant infos)
  (define dep (coerce-zcpkg-query dep-variant))
  (define revisions (filter-revisions dep infos))
  (define sorted (sort-revisions (sequence->list revisions)))
  (define min-revision-number (find-revision-number (zcpkg-query-revision-min dep) sorted))
  (define max-revision-number (find-revision-number (zcpkg-query-revision-max dep) sorted))

  (if (and min-revision-number max-revision-number)
      (let-values ([(min-adjusted max-adjusted)
                    (get-zcpkg-query-revision-range dep
                                                    #:named-interval (zcpkg-query->string dep)
                                                    #:lo min-revision-number
                                                    #:hi max-revision-number)])
        (sequence-filter (λ (info)
                           (in? (zcpkg-info-revision-number info)
                                min-adjusted
                                max-adjusted))
                         sorted))
      empty-sequence))


; Check if a well-formed zcpkg-query encompasses an exact zcpkg-query.
(define (zcpkg-query-match? to-match/variant exact-dep/variant)
  (define to-match (coerce-zcpkg-query to-match/variant))
  (define exact-dep (coerce-zcpkg-query exact-dep/variant))

  (unless (concrete-zcpkg-query? to-match)
    (raise-argument-error 'zcpkg-query-match?
                          "A concrete zcpkg-query declaration"
                          0
                          to-match/variant
                          exact-dep/variant))

  (unless (exact-zcpkg-query? exact-dep)
    (raise-argument-error 'zcpkg-query-match?
                          "An exact zcpkg-query declaration"
                          1
                          to-match/variant
                          exact-dep/variant))

  (define-values (l1 h1)
    (get-zcpkg-query-revision-range #:named-interval "haystack zcpkg-query"
                                    exact-dep))
  (define-values (l2 h2)
    (get-zcpkg-query-revision-range #:named-interval "needle zcpkg-query"
                                    to-match))

  (and (zcpkg-query-identity=? exact-dep to-match)
       (revision-range-subset? l1 h1 l2 h2)))

(define (zcpkg-query-identity=? a b)
  (define x (coerce-zcpkg-query a))
  (define y (coerce-zcpkg-query b))
  (andmap (λ (p) (equal? (p x) (p y)))
          (list zcpkg-query-provider-name
                zcpkg-query-package-name
                zcpkg-query-edition-name)))

(define (coerce-zcpkg-query v)
  (cond [(zcpkg-query? v) v]
        [(string? v) (string->zcpkg-query v)]
        [(zcpkg-info? v) (zcpkg-info->zcpkg-query v)]))

(define (zcpkg-query-string? s)
  (with-handlers ([(const #t) (const #f)])
    (and (string->zcpkg-query s) #t)))

(define (string->zcpkg-query s)
  (match (string-split s ":")
    [(list (? name-string? provider-name)
           (? name-string? package-name))
     (zcpkg-query provider-name
                  package-name
                  "draft"
                  #f "newest"
                  #f "newest")]

    [(list (? name-string? provider-name)
           (? name-string? package-name)
           (? name-string? edition-name))
     (zcpkg-query provider-name
                  package-name
                  edition-name
                  #f "newest"
                  #f "newest")]

    [(list (? name-string? provider-name)
           (? name-string? package-name)
           (? name-string? edition-name)
           (? revision-string? revision))
     (zcpkg-query provider-name
                  package-name
                  edition-name
                  #f revision
                  #f revision)]

    [(list (? name-string? provider-name)
           (? name-string? package-name)
           (? name-string? edition-name)
           (? revision-string? min-revision)
           (? revision-string? max-revision))
     (zcpkg-query provider-name
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
     (zcpkg-query provider-name
                  package-name
                  edition-name
                  (exclusive-flag->bool min-flag "revision minimum flag")
                  min-revision
                  (exclusive-flag->bool max-flag "revision maximum flag")
                  max-revision)]

    [_ (raise-user-error (format "~s is not a valid package query string." s))]))

(define (zcpkg-info->zcpkg-query info)
  (zcpkg-query (zcpkg-info-provider-name info)
               (zcpkg-info-package-name info)
               (zcpkg-info-edition-name info)
               #f
               (zcpkg-info-revision-number info)
               #f
               (zcpkg-info-revision-number info)))

(define (zcpkg-query->string d)
  (string-join
   (list (zcpkg-query-provider-name d)
         (zcpkg-query-package-name d)
         (zcpkg-query-edition-name d)
         (bool->exclusive-flag (zcpkg-query-revision-min-exclusive? d))
         (~a (zcpkg-query-revision-min d))
         (bool->exclusive-flag (zcpkg-query-revision-max-exclusive? d))
         (~a (zcpkg-query-revision-max d)))
   ":"))

(define (exclusive-flag->bool flag name)
  (match flag
    ["i" #f]
    ["e" #t]
    [_ (raise-argument-error 'string->zcpkg-query
                             (format "\"i\" or \"e\" for ~a" name)
                             flag)]))

(define (bool->exclusive-flag expr)
  (if expr "e" "i"))

(define (filter-revisions dep infos)
  (sequence-filter (λ (info)
                     (zcpkg-non-revision-identity=?
                      info
                      (zcpkg-query-provider-name dep)
                      (zcpkg-query-package-name dep)
                      (zcpkg-query-edition-name dep)))
                   infos))

(define (sort-revisions infos)
  (sort infos (λ (a b) (> (zcpkg-compare-versions a b) 0))))

(define (in? n l h)
  (and (>= n l)
       (<= n h)))

(define (revision-range-subset? l1 h1 l2 h2)
  (and (in? l1 l2 h2)
       (in? h1 l2 h2)))

(define (get-zcpkg-query-revision-range d
                                        #:named-interval [named-interval #f]
                                        #:lo [lo (zcpkg-query-revision-min d)]
                                        #:hi [hi (zcpkg-query-revision-max d)])
  (get-inclusive-revision-range #:named-interval named-interval
                                (zcpkg-query-revision-min-exclusive? d)
                                (zcpkg-query-revision-max-exclusive? d)
                                lo
                                hi))

(define (get-inclusive-revision-range #:named-interval [named-interval #f]
                                      min-exclusive?
                                      max-exclusive?
                                      lo hi)
  (define (add1-if do-it? low-num)
    (if do-it? (add1 low-num) low-num))
  (define (sub1-if do-it? hi-num)
    (if do-it? (sub1 hi-num) hi-num))

  (define adjusted-lo (add1-if min-exclusive? (coerce-revision-number lo)))
  (define adjusted-hi (sub1-if max-exclusive? (coerce-revision-number hi)))

  (assert-valid-revision-range adjusted-lo adjusted-hi named-interval)

  (values adjusted-lo
          adjusted-hi))


; Assumes infos are sorted from newest to oldest.
(define (find-revision-number variant infos)
  (cond [(= (sequence-length infos) 0)
         #f]
        [(revision-number? variant) variant]
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
    (list (zcpkg-query "" "package" "edition" #t "0" #t "0")
          (zcpkg-query "provider" "" "edition" #t "0" #t "0")
          (zcpkg-query "provider" "edition" "" #t "0" #t "0")
          (zcpkg-query "provider" "package" "edition" "#t" "0" #t "0")
          (zcpkg-query "provider" "package" "edition" #t #f #t "0")
          (zcpkg-query "provider" "package" "edition" #t "0" "#t" "0")
          (zcpkg-query "provider" "package" "edition" #t "0" #t #f)
          (zcpkg-query #f #f #f #f #f #f #f)))

  (define (zpi pr pk ed n rns)
    (make-zcpkg-info #:provider-name pr
                     #:package-name pk
                     #:edition-name ed
                     #:revision-number n
                     #:revision-names rns))


  (define (test-true* p seq)
    (for ([args (in-values-sequence seq)])
      (test-true (format "Expect: (eq? #t (~a ~a))"
                         (object-name p)
                         (string-join (map ~v args)
                                      " "))
                 (apply p args))))

  (test-pred "Detect well-formed dependencies"
             well-formed-zcpkg-query?
             (zcpkg-query "provider" "package" "edition" #t "0" #t "0"))

  (for ([example (in-list ill-formed)])
    (test-false (format "Detect ill-formed zcpkg-query: ~s" example)
                (well-formed-zcpkg-query? example)))

  (test-true "Detect concrete zcpkg-query"
             (concrete-zcpkg-query? (zcpkg-query "provider" "package" "edition" #t "0" #t "10")))
  (test-false "Detect abstract zcpkg-query"
              (concrete-zcpkg-query? (zcpkg-query "provider" "package" "edition" #t "initial" #t "10")))

  (test-true "Exact zcpkg-query: One possible version"
             (exact-zcpkg-query? (zcpkg-query "p" "p" "p" #f "0" #f "0")))
  (test-false "Prohibit exact zcpkg-query from having exclusive lower bound"
              (exact-zcpkg-query? (zcpkg-query "p" "p" "p" #t "0" #f "0")))
  (test-false "Prohibit exact zcpkg-query from having exclusive upper bound"
              (exact-zcpkg-query? (zcpkg-query "p" "p" "p" #f "0" #t "0")))
  (test-false "Prohibit exact zcpkg-query from varying on version"
              (exact-zcpkg-query? (zcpkg-query "p" "p" "p" #f "0" #f "1")))

  (test-case "Convert between zcpkg-query instances and their representations"
    (define target (zcpkg-query "joe" "pkg" "edition" #f "8" #f "8"))
    (define str-repr "joe:pkg:edition:i:8:i:8")
    (define info-repr (zpi "joe" "pkg" "edition" "8" null))
    (check-equal? target (string->zcpkg-query str-repr))
    (check-equal? target (coerce-zcpkg-query str-repr))
    (check-equal? target (zcpkg-info->zcpkg-query info-repr))
    (check-equal? target (coerce-zcpkg-query info-repr))
    (check-equal? str-repr (zcpkg-query->string target)))


  (test-true "Detect equal zcpkg-query identities"
             (zcpkg-query-identity=? (zcpkg-query "a" "b" "c" #f #f #f #f)
                                     (zcpkg-query "a" "b" "c" #f #f #f #f)))

  (test-false "Detect differing provider names"
              (zcpkg-query-identity=? (zcpkg-query "a" "b" "c" #f #f #f #f)
                                      (zcpkg-query " " "b" "c" #f #f #f #f)))

  (test-false "Detect differing package names"
              (zcpkg-query-identity=? (zcpkg-query "a" "b" "c" #f #f #f #f)
                                      (zcpkg-query "a" " " "c" #f #f #f #f)))

  (test-false "Detect differing edition names"
              (zcpkg-query-identity=? (zcpkg-query "a" "b" "c" #f #f #f #f)
                                      (zcpkg-query "a" "b" " " #f #f #f #f)))


  (test-case "Use strings to produce exact and inexact dependencies"
    (define (check-conversion dep str)
      (check-equal? dep (string->zcpkg-query str)))

    (check-conversion (zcpkg-query "joe" "pkg" "draft" #f "newest" #f "newest")
                      "joe:pkg")

    (check-conversion (zcpkg-query "joe" "pkg" "edition" #f "newest" #f "newest")
                      "joe:pkg:edition")

    (check-conversion (zcpkg-query "joe" "pkg" "edition" #f "initial" #f "initial")
                      "joe:pkg:edition:initial")

    (check-conversion (zcpkg-query "joe" "pkg" "edition" #f "min" #f "max")
                      "joe:pkg:edition:min:max")

    (check-conversion (zcpkg-query "joe" "pkg" "edition" #f "beta" #t "prod")
                      "joe:pkg:edition:i:beta:e:prod"))

  (test-pred "Use zcpkg-info instances to produce exact dependencies"
             exact-zcpkg-query?
             (zcpkg-info->zcpkg-query (zpi "joe" "pkg" "edition" "8" null)))


  (test-true "Match a zcpkg-query using a revision range"
             (zcpkg-query-match? "joe:pkg:draft:0:100" "joe:pkg:draft:0:0"))

  (test-true "Match a zcpkg-query using a revision range, even with string+number mixes"
             (zcpkg-query-match? (zcpkg-query "joe" "pkg" "draft" #f "0" #f 100)
                                 (zcpkg-query "joe" "pkg" "draft" #f 0 #f "0")))

  (test-true "Match a zcpkg-query exactly"
             (zcpkg-query-match? "joe:pkg:draft:2:2" "joe:pkg:draft:2:2"))

  (test-true "Match a zcpkg-query that risks an off-by-one error (lower bound)"
             (zcpkg-query-match? "joe:pkg:draft:e:1:i:3"
                                 "joe:pkg:draft:2:2"))

  (test-true "Match a zcpkg-query that risks an off-by-one error (upper bound)"
             (zcpkg-query-match? "joe:pkg:draft:i:1:e:3"
                                 "joe:pkg:draft:2:2"))

  (test-false "Do not match a zcpkg-query that differs in provider name"
              (zcpkg-query-match? "joe:pkg:draft:i:1:e:3"
                                  "je:pkg:draft:2:2"))

  (test-false "Do not match a zcpkg-query that differs in package name"
              (zcpkg-query-match? "joe:pkg:draft:i:1:e:3"
                                  "joe:pg:draft:2:2"))

  (test-false "Do not match a zcpkg-query that differs in edition name"
              (zcpkg-query-match? "joe:pkg:draft:i:1:e:3"
                                  "joe:pkg:drft:2:2"))

  (test-exn "Raise a contract error if comparing two inexact dependencies"
            #rx"expected: An exact zcpkg-query"
            (λ () (zcpkg-query-match? "joe:pkg:draft:i:1:e:3"
                                      "joe:pkg")))

  (test-exn "Raise a contract error if matching against an ill-formed zcpkg-query"
            #rx"expected: A concrete zcpkg-query"
            (λ () (zcpkg-query-match? (zcpkg-query #f #f #f #f #f #f #f)
                                      "joe:pkg:draft:1:1")))

  (test-exn "Raise a contract error if matching against an abstract zcpkg-query"
            #rx"expected: A concrete zcpkg-query"
            (λ () (zcpkg-query-match? "joe:pkg"
                                      "joe:pkg:draft:1:1")))

  (test-exn "Raise a special error if matching against an invalid interval"
            exn:fail:zcpkg:invalid-revision-interval?
            (λ () (zcpkg-query-match? "joe:pkg:draft:10:1"
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

  (test-case "Create inclusive revision ranges"
    (define (check-range expected-lo expected-hi . args)
      (define-values (actual-lo actual-hi) (apply get-inclusive-revision-range args))
      (check-equal? actual-lo expected-lo)
      (check-equal? actual-hi expected-hi))

    (check-range 0 0 #f #f 0 0)
    (check-range 1 2 #t #t 0 3)
    (check-range 1 2 #t #f 0 2)
    (check-range 1 2 #f #t 1 3))

  (test-case "Metadata search"
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
      (filter-revisions (coerce-zcpkg-query "alice:wonderland:mad") infos))
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

    (test-eq? "Search will fail on empty sequence"
              (find-revision-number "initial" null)
              #f)

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
