#lang racket/base

; Define a data type to match against package definitions.

(require "contract.rkt")

(provide (struct-out xiden-query)
         (contract-out
          [revision-string? predicate/c]
          [revision-number? predicate/c]
          [revision-number-string? predicate/c]
          [well-formed-xiden-query/c flat-contract?]
          [xiden-query-string? predicate/c]
          [xiden-query-variant? predicate/c]
          [coerce-xiden-query (-> xiden-query-variant? xiden-query?)]
          [xiden-query->string (-> xiden-query? string?)]
          [package-evaluator->xiden-query (-> (-> any/c any) xiden-query?)]
          [get-xiden-query-revision-range
           (->* (xiden-query?)
                (#:lo revision-number?
                 #:hi revision-number?
                 #:named-interval (or/c #f string?))
                (values revision-number? revision-number?))]
          [abbreviate-exact-xiden-query
           (-> xiden-query? string?)]
          [string->xiden-query (-> string? xiden-query?)]))


(require racket/format
         racket/function
         racket/match
         racket/generator
         racket/match
         racket/sequence
         "exn.rkt"
         "sandbox.rkt"
         "string.rkt")

(define-exn exn:fail:xiden:invalid-revision-interval exn:fail:xiden (lo hi))

(define revision-number-pattern-string "\\d+")

(define revision-number-string?
  (make-rx-predicate revision-number-pattern-string))

(define revision-number?
  exact-nonnegative-integer?)

(define revision-string?
  (or/c revision-number-string?
        file-name-string?))

(define (coerce-revision-number n)
  (if (revision-number-string? n)
      (string->number n)
      n))

(struct xiden-query
  (provider-name
   package-name
   edition-name
   revision-min
   revision-max
   interval-bounds)
  #:transparent)

(define well-formed-xiden-query/c
  (struct/c xiden-query
            non-empty-string?
            non-empty-string?
            string?
            string?
            string?
            (or/c "" "ii" "ee" "ie" "ei")))


(define (xiden-query-variant? v)
  ((disjoin string? xiden-query? procedure?) v))


(define (coerce-xiden-query v)
  (cond [(xiden-query? v) v]
        [(string? v) (string->xiden-query v)]
        [(procedure? v) (package-evaluator->xiden-query v)]))


(define (package-evaluator->xiden-query pkgeval)
  (xiden-query (xiden-evaluator-ref pkgeval 'provider "")
               (xiden-evaluator-ref pkgeval 'package "")
               (xiden-evaluator-ref pkgeval 'edition "")
               (~a (xiden-evaluator-ref pkgeval 'revision-number ""))
               (~a (xiden-evaluator-ref pkgeval 'revision-number ""))
               "ii"))


(define (query-ref s def)
  (if (non-empty-string? s)
      s
      def))


(define (xiden-query-string? s)
  (with-handlers ([values (const #f)])
    (well-formed-xiden-query/c (string->xiden-query s))))


(define (string->xiden-query s)
  (define user-defined (string-split s ":"))
  (define num-fields (length user-defined))
  (apply xiden-query
         (build-list (procedure-arity xiden-query)
                     (λ (i)
                       (if (< i num-fields)
                           (list-ref user-defined i)
                           "")))))


(define (xiden-query->string d)
  (regexp-replace* ":+$"
                   (string-join (cdr (vector->list (struct->vector d)))
                                ":")
                   ""))

(define (abbreviate-exact-xiden-query q)
  (string-join (map (λ (acc) (acc q))
                    (list xiden-query-provider-name
                          xiden-query-package-name
                          xiden-query-edition-name
                          xiden-query-revision-min))
               ":"))


(define (get-xiden-query-revision-range d
                                        #:named-interval [named-interval #f]
                                        #:lo [lo (string->number (xiden-query-revision-min d))]
                                        #:hi [hi (string->number (xiden-query-revision-max d))])
  (define bounds (query-ref (xiden-query-interval-bounds d) "ii"))
  (define (->bool i)
    (define flag (string-ref bounds i))
    (match flag
      [#\i #f]
      [#\e #t]
      [_ (raise-argument-error 'string->xiden-query
                               (format "\"i\" or \"e\" for ~a" named-interval)
                               flag)]))
  (get-inclusive-revision-range #:named-interval named-interval
                                (->bool 0)
                                (->bool 1)
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

  (test-equal? "Abbreviate queries"
               (abbreviate-exact-xiden-query (xiden-query "a" "b" "c" "0" "0" "ii"))
               "a:b:c:0")

  (test-case "Convert between queries and strings"
    (define (verify s expected) (check-equal? (xiden-query->string (string->xiden-query s)) expected))
    (verify ""                "")
    (verify "a"               "a")
    (verify "a:b"             "a:b")
    (verify "a:b:c"           "a:b:c")
    (verify "a:b:c:d"         "a:b:c:d")
    (verify "a:b:c:d:e"       "a:b:c:d:e")
    (verify "a:b:c:d:e:f"     "a:b:c:d:e:f")
    (verify "a:b:c:d:e:f:g"   "a:b:c:d:e:f")
    (verify "a:b:c::e:f:g:h"  "a:b:c::e:f"))

  (define (test-true* p seq)
    (for ([args (in-values-sequence seq)])
      (test-true (format "Expect: (eq? #t (~a ~a))"
                         (object-name p)
                         (string-join (map ~v args)
                                      " "))
                 (apply p args))))

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
