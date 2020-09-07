#lang racket/base

; Define a data type to match against package definitions.

(require "contract.rkt")

(provide (struct-out xiden-query)
         (contract-out
          [CONVENTIONAL_NEWEST_REVISION_NAME string?]
          [revision-string? predicate/c]
          [revision-number? predicate/c]
          [revision-number-string? predicate/c]
          [well-formed-xiden-query? predicate/c]
          [concrete-xiden-query? predicate/c]
          [exact-xiden-query? predicate/c]
          [xiden-query-string? predicate/c]
          [xiden-query-variant? predicate/c]
          [xiden-query-identity=? (-> xiden-query-variant? xiden-query-variant? boolean?)]
          [coerce-xiden-query (-> xiden-query-variant? xiden-query?)]
          [xiden-query->string (-> xiden-query? string?)]
          [get-inclusive-revision-range
           (->* (any/c any/c revision-number? revision-number?)
                (#:named-interval (or/c #f string?))
                (values revision-number? revision-number?))]
          [string->xiden-query (-> string? xiden-query?)]
          [xiden-query-match? (-> xiden-query-variant? xiden-query-variant? boolean?)]))


(require racket/format
         racket/function
         racket/match
         racket/generator
         racket/match
         racket/sequence
         "exn.rkt"
         "string.rkt")

(define-exn exn:fail:xiden:invalid-revision-interval exn:fail:xiden (lo hi))

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

(struct xiden-query
  (provider-name
   package-name
   edition-name
   revision-min-exclusive?
   revision-min
   revision-max-exclusive?
   revision-max)
  #:transparent)

; Define "Well-formed" to mean that a xiden-query structure
; has correct value types for referencing some package.
(define (well-formed-xiden-query? v)
  (passes-invariant-assertion?
   (struct/c xiden-query
             name-string?
             name-string?
             name-string?
             boolean?
             (or/c revision-string? revision-number?)
             boolean?
             (or/c revision-string? revision-number?))
   v))

; Define a "concrete" xiden-query as a well-formed xiden-query
; with a revision number range for some package.
(define (concrete-xiden-query? v)
  (passes-invariant-assertion?
   (struct/c xiden-query
             name-string?
             name-string?
             name-string?
             boolean?
             (or/c revision-number-string? revision-number?)
             boolean?
             (or/c revision-number-string? revision-number?))
   v))

; Recognize the many sources of a xiden-query declarations.
(define (xiden-query-variant? v)
  ((disjoin string?
            xiden-query?) v))

; Define an exact xiden-query as a concrete xiden-query where the
; revision number range has exactly one element.  Assuming a
; provider's identity is not compromised, an exact xiden-query is
; globally unique.
(define (exact-xiden-query? d)
  (and (concrete-xiden-query? d)
       (not (xiden-query-revision-min-exclusive? d))
       (not (xiden-query-revision-max-exclusive? d))
       (equal? (coerce-revision-number (xiden-query-revision-min d))
               (coerce-revision-number (xiden-query-revision-max d)))))

; Check if a well-formed xiden-query encompasses an exact xiden-query.
(define (xiden-query-match? to-match/variant exact-dep/variant)
  (define to-match (coerce-xiden-query to-match/variant))
  (define exact-dep (coerce-xiden-query exact-dep/variant))

  (unless (concrete-xiden-query? to-match)
    (raise-argument-error 'xiden-query-match?
                          "A concrete xiden-query declaration"
                          0
                          to-match/variant
                          exact-dep/variant))

  (unless (exact-xiden-query? exact-dep)
    (raise-argument-error 'xiden-query-match?
                          "An exact xiden-query declaration"
                          1
                          to-match/variant
                          exact-dep/variant))

  (define-values (l1 h1)
    (get-xiden-query-revision-range #:named-interval "haystack xiden-query"
                                    exact-dep))
  (define-values (l2 h2)
    (get-xiden-query-revision-range #:named-interval "needle xiden-query"
                                    to-match))

  (and (xiden-query-identity=? exact-dep to-match)
       (revision-range-subset? l1 h1 l2 h2)))

(define (xiden-query-identity=? a b)
  (define x (coerce-xiden-query a))
  (define y (coerce-xiden-query b))
  (andmap (λ (p) (equal? (p x) (p y)))
          (list xiden-query-provider-name
                xiden-query-package-name
                xiden-query-edition-name)))

(define (coerce-xiden-query v)
  (cond [(xiden-query? v) v]
        [(string? v) (string->xiden-query v)]))

(define (xiden-query-string? s)
  (with-handlers ([(const #t) (const #f)])
    (and (string->xiden-query s) #t)))

(define (string->xiden-query s)
  (match (string-split s ":")
    [(list (? name-string? provider-name)
           (? name-string? package-name))
     (xiden-query provider-name
                  package-name
                  "draft"
                  #f "newest"
                  #f "newest")]

    [(list (? name-string? provider-name)
           (? name-string? package-name)
           (? name-string? edition-name))
     (xiden-query provider-name
                  package-name
                  edition-name
                  #f "newest"
                  #f "newest")]

    [(list (? name-string? provider-name)
           (? name-string? package-name)
           (? name-string? edition-name)
           (? revision-string? revision))
     (xiden-query provider-name
                  package-name
                  edition-name
                  #f revision
                  #f revision)]

    [(list (? name-string? provider-name)
           (? name-string? package-name)
           (? name-string? edition-name)
           (? revision-string? min-revision)
           (? revision-string? max-revision))
     (xiden-query provider-name
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
     (xiden-query provider-name
                  package-name
                  edition-name
                  (exclusive-flag->bool min-flag "revision minimum flag")
                  min-revision
                  (exclusive-flag->bool max-flag "revision maximum flag")
                  max-revision)]

    [_ (raise-user-error (format "~s is not a valid package query string." s))]))


(define (xiden-query->string d)
  (string-join
   (list (xiden-query-provider-name d)
         (xiden-query-package-name d)
         (xiden-query-edition-name d)
         (bool->exclusive-flag (xiden-query-revision-min-exclusive? d))
         (~a (xiden-query-revision-min d))
         (bool->exclusive-flag (xiden-query-revision-max-exclusive? d))
         (~a (xiden-query-revision-max d)))
   ":"))

(define (exclusive-flag->bool flag name)
  (match flag
    ["i" #f]
    ["e" #t]
    [_ (raise-argument-error 'string->xiden-query
                             (format "\"i\" or \"e\" for ~a" name)
                             flag)]))

(define (bool->exclusive-flag expr)
  (if expr "e" "i"))

(define (in? n l h)
  (and (>= n l)
       (<= n h)))

(define (revision-range-subset? l1 h1 l2 h2)
  (and (in? l1 l2 h2)
       (in? h1 l2 h2)))

(define (get-xiden-query-revision-range d
                                        #:named-interval [named-interval #f]
                                        #:lo [lo (xiden-query-revision-min d)]
                                        #:hi [hi (xiden-query-revision-max d)])
  (get-inclusive-revision-range #:named-interval named-interval
                                (xiden-query-revision-min-exclusive? d)
                                (xiden-query-revision-max-exclusive? d)
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


(define (assert-valid-revision-range lo hi [named-interval #f])
  (when (< hi lo)
    (raise
     (exn:fail:xiden:invalid-revision-interval
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
    (list (xiden-query "" "package" "edition" #t "0" #t "0")
          (xiden-query "provider" "" "edition" #t "0" #t "0")
          (xiden-query "provider" "edition" "" #t "0" #t "0")
          (xiden-query "provider" "package" "edition" "#t" "0" #t "0")
          (xiden-query "provider" "package" "edition" #t #f #t "0")
          (xiden-query "provider" "package" "edition" #t "0" "#t" "0")
          (xiden-query "provider" "package" "edition" #t "0" #t #f)
          (xiden-query #f #f #f #f #f #f #f)))


  (define (test-true* p seq)
    (for ([args (in-values-sequence seq)])
      (test-true (format "Expect: (eq? #t (~a ~a))"
                         (object-name p)
                         (string-join (map ~v args)
                                      " "))
                 (apply p args))))

  (test-pred "Detect well-formed dependencies"
             well-formed-xiden-query?
             (xiden-query "provider" "package" "edition" #t "0" #t "0"))

  (for ([example (in-list ill-formed)])
    (test-false (format "Detect ill-formed xiden-query: ~s" example)
                (well-formed-xiden-query? example)))

  (test-true "Detect concrete xiden-query"
             (concrete-xiden-query? (xiden-query "provider" "package" "edition" #t "0" #t "10")))
  (test-false "Detect abstract xiden-query"
              (concrete-xiden-query? (xiden-query "provider" "package" "edition" #t "initial" #t "10")))

  (test-true "Exact xiden-query: One possible version"
             (exact-xiden-query? (xiden-query "p" "p" "p" #f "0" #f "0")))
  (test-false "Prohibit exact xiden-query from having exclusive lower bound"
              (exact-xiden-query? (xiden-query "p" "p" "p" #t "0" #f "0")))
  (test-false "Prohibit exact xiden-query from having exclusive upper bound"
              (exact-xiden-query? (xiden-query "p" "p" "p" #f "0" #t "0")))
  (test-false "Prohibit exact xiden-query from varying on version"
              (exact-xiden-query? (xiden-query "p" "p" "p" #f "0" #f "1")))

  (test-case "Convert between xiden-query instances and their representations"
    (define target (xiden-query "joe" "pkg" "edition" #f "8" #f "8"))
    (define str-repr "joe:pkg:edition:i:8:i:8")
    (check-equal? target (string->xiden-query str-repr))
    (check-equal? target (coerce-xiden-query str-repr))
    (check-equal? str-repr (xiden-query->string target)))


  (test-true "Detect equal xiden-query identities"
             (xiden-query-identity=? (xiden-query "a" "b" "c" #f #f #f #f)
                                     (xiden-query "a" "b" "c" #f #f #f #f)))

  (test-false "Detect differing provider names"
              (xiden-query-identity=? (xiden-query "a" "b" "c" #f #f #f #f)
                                      (xiden-query " " "b" "c" #f #f #f #f)))

  (test-false "Detect differing package names"
              (xiden-query-identity=? (xiden-query "a" "b" "c" #f #f #f #f)
                                      (xiden-query "a" " " "c" #f #f #f #f)))

  (test-false "Detect differing edition names"
              (xiden-query-identity=? (xiden-query "a" "b" "c" #f #f #f #f)
                                      (xiden-query "a" "b" " " #f #f #f #f)))


  (test-case "Use strings to produce exact and inexact dependencies"
    (define (check-conversion dep str)
      (check-equal? dep (string->xiden-query str)))

    (check-conversion (xiden-query "joe" "pkg" "draft" #f "newest" #f "newest")
                      "joe:pkg")

    (check-conversion (xiden-query "joe" "pkg" "edition" #f "newest" #f "newest")
                      "joe:pkg:edition")

    (check-conversion (xiden-query "joe" "pkg" "edition" #f "initial" #f "initial")
                      "joe:pkg:edition:initial")

    (check-conversion (xiden-query "joe" "pkg" "edition" #f "min" #f "max")
                      "joe:pkg:edition:min:max")

    (check-conversion (xiden-query "joe" "pkg" "edition" #f "beta" #t "prod")
                      "joe:pkg:edition:i:beta:e:prod"))

  (test-true "Match a xiden-query using a revision range"
             (xiden-query-match? "joe:pkg:draft:0:100" "joe:pkg:draft:0:0"))

  (test-true "Match a xiden-query using a revision range, even with string+number mixes"
             (xiden-query-match? (xiden-query "joe" "pkg" "draft" #f "0" #f 100)
                                 (xiden-query "joe" "pkg" "draft" #f 0 #f "0")))

  (test-true "Match a xiden-query exactly"
             (xiden-query-match? "joe:pkg:draft:2:2" "joe:pkg:draft:2:2"))

  (test-true "Match a xiden-query that risks an off-by-one error (lower bound)"
             (xiden-query-match? "joe:pkg:draft:e:1:i:3"
                                 "joe:pkg:draft:2:2"))

  (test-true "Match a xiden-query that risks an off-by-one error (upper bound)"
             (xiden-query-match? "joe:pkg:draft:i:1:e:3"
                                 "joe:pkg:draft:2:2"))

  (test-false "Do not match a xiden-query that differs in provider name"
              (xiden-query-match? "joe:pkg:draft:i:1:e:3"
                                  "je:pkg:draft:2:2"))

  (test-false "Do not match a xiden-query that differs in package name"
              (xiden-query-match? "joe:pkg:draft:i:1:e:3"
                                  "joe:pg:draft:2:2"))

  (test-false "Do not match a xiden-query that differs in edition name"
              (xiden-query-match? "joe:pkg:draft:i:1:e:3"
                                  "joe:pkg:drft:2:2"))

  (test-exn "Raise a contract error if comparing two inexact dependencies"
            #rx"expected: An exact xiden-query"
            (λ () (xiden-query-match? "joe:pkg:draft:i:1:e:3"
                                      "joe:pkg")))

  (test-exn "Raise a contract error if matching against an ill-formed xiden-query"
            #rx"expected: A concrete xiden-query"
            (λ () (xiden-query-match? (xiden-query #f #f #f #f #f #f #f)
                                      "joe:pkg:draft:1:1")))

  (test-exn "Raise a contract error if matching against an abstract xiden-query"
            #rx"expected: A concrete xiden-query"
            (λ () (xiden-query-match? "joe:pkg"
                                      "joe:pkg:draft:1:1")))

  (test-exn "Raise a special error if matching against an invalid interval"
            exn:fail:xiden:invalid-revision-interval?
            (λ () (xiden-query-match? "joe:pkg:draft:10:1"
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
    (check-range 1 2 #f #t 1 3)))
