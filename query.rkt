#lang racket/base

; Define a data type to match against package definitions.

(require "contract.rkt")

(provide (struct-out xiden-query)
         (contract-out
          [well-formed-xiden-query? predicate/c]
          [resolved-xiden-query? predicate/c]
          [malformed-xiden-query? predicate/c]
          [exact-xiden-query? predicate/c]
          [xiden-query-string? predicate/c]
          [xiden-query-variant? predicate/c]
          [coerce-xiden-query (-> xiden-query-variant? xiden-query?)]
          [xiden-query->string (-> well-formed-xiden-query? string?)]
          [package-evaluator->xiden-query (-> (-> any/c any) xiden-query?)]
          [abbreviate-exact-xiden-query (-> exact-xiden-query? string?)]
          [get-resolved-revision-interval (-> resolved-xiden-query?
                                              (values revision-number?
                                                      revision-number?))]
          [string->xiden-query (-> string? xiden-query?)]))


(require racket/function
         racket/match
         "format.rkt"
         "logged.rkt"
         "message.rkt"
         "sandbox.rkt"
         "string.rkt"
         "version.rkt")

(struct xiden-query
  (provider-name
   package-name
   edition-name
   revision-min
   revision-max
   interval-bounds)
  #:transparent)


(define well-formed-xiden-query?
  (struct/c xiden-query
            non-empty-string?
            non-empty-string?
            string?
            string?
            string?
            (or/c "" "ii" "ee" "ie" "ei")))


(define malformed-xiden-query?
  (negate well-formed-xiden-query?))


(define (resolved-xiden-query? v)
  (and (well-formed-xiden-query? v)
       (revision-number-string? (xiden-query-revision-min v))
       (revision-number-string? (xiden-query-revision-max v))))


(define (exact-xiden-query? v)
  (and (resolved-xiden-query? v)
       (equal? (xiden-query-revision-min v)
               (xiden-query-revision-max v))
       (equal? (xiden-query-interval-bounds v)
               "ii")))


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
    (well-formed-xiden-query? (string->xiden-query s))))


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


(define (boundary-flag->boolean flag)
  (case flag
    [(#\i) #f]
    [(#\e) #t]
    [else (raise-argument-error 'boundary-flag->boolean
                                "#\\i or #\\e"
                                flag)]))

(define (boolean->boundary-flag flag)
  (if flag #\e #\i))


(define (get-resolved-revision-interval query)
  (make-revision-interval
   (string->number (xiden-query-revision-min query))
   (string->number (xiden-query-revision-max query))
   #:lo-exclusive (boundary-flag->boolean (string-ref (xiden-query-interval-bounds query) 0))
   #:hi-exclusive (boundary-flag->boolean (string-ref (xiden-query-interval-bounds query) 1))))


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
    (verify "a:b:c::e:f:g:h"  "a:b:c::e:f")))
