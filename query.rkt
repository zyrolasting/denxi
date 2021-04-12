#lang racket/base

; Define a data type to match against package definitions.

(require racket/function
         racket/match
         "contract.rkt"
         "format.rkt"
         "string.rkt"
         "version.rkt")

(provide (struct-out parsed-package-query)
         (contract-out
          [boundary-flags-string? predicate/c]
          [well-formed-package-query? predicate/c]
          [resolved-package-query? predicate/c]
          [malformed-package-query? predicate/c]
          [exact-package-query? predicate/c]
          [package-query? predicate/c]
          [package-query-variant? predicate/c]
          [coerce-parsed-package-query (-> package-query-variant? parsed-package-query?)]
          [format-parsed-package-query (-> well-formed-package-query? string?)]
          [abbreviate-exact-package-query (-> exact-package-query? string?)]
          [make-exact-package-query
           (-> string? string? string? revision-number? exact-package-query?)]
          [parse-package-query (-> package-query? parsed-package-query?)]
          [resolve-revision-interval (->* (parsed-package-query? (-> boolean? string?
                                                                     (or/c #f revision-number-variant?)))
                                          (#:default-bounds boundary-flags-string?)
                                          (values revision-number?
                                                  revision-number?))]))

(struct parsed-package-query
  (provider-name
   package-name
   edition-name
   revision-min
   revision-max
   interval-bounds)
  #:transparent)


(define (boundary-flags-string? s)
  (with-handlers ([exn? (λ (e) #f)])
    (boundary-flag->boolean (string-ref s 0))
    (boundary-flag->boolean (string-ref s 1))
    #t))


(define well-formed-package-query?
  (struct/c parsed-package-query
            string?
            string?
            string?
            string?
            string?
            (or/c "" boundary-flags-string?)))

(define malformed-package-query?
  (negate well-formed-package-query?))


(define (resolved-package-query? v)
  (and (well-formed-package-query? v)
       (revision-number-string? (parsed-package-query-revision-min v))
       (revision-number-string? (parsed-package-query-revision-max v))))


(define (exact-package-query? v)
  (and (resolved-package-query? v)
       (equal? (parsed-package-query-revision-min v)
               (parsed-package-query-revision-max v))
       (equal? (parsed-package-query-interval-bounds v)
               "ii")))


(define (package-query-variant? v)
  ((disjoin package-query? parsed-package-query?) v))


(define (coerce-parsed-package-query v)
  (cond [(parsed-package-query? v) v]
        [(package-query? v) (parse-package-query v)]))


(define (query-ref s def)
  (if (non-empty-string? s)
      s
      def))


(define (package-query? s)
  (with-handlers ([values (const #f)])
    (well-formed-package-query? (parse-package-query s))))


(define (parse-package-query s)
  (define user-defined (string-split #:trim? #f #:repeat? #f s ":"))
  (define num-fields (length user-defined))
  (apply parsed-package-query
         (build-list (procedure-arity parsed-package-query)
                     (λ (i)
                       (if (< i num-fields)
                           (list-ref user-defined i)
                           "")))))


(define (format-parsed-package-query d)
  (regexp-replace* ":+$"
                   (string-join (cdr (vector->list (struct->vector d)))
                                ":")
                   ""))

(define (abbreviate-exact-package-query q)
  (string-join (map (λ (acc) (acc q))
                    (list parsed-package-query-provider-name
                          parsed-package-query-package-name
                          parsed-package-query-edition-name
                          parsed-package-query-revision-min))
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


(define (resolve-revision-interval #:default-bounds [default-bounds "ii"] query revision->revision-number)
  (let ([bounds (if (boundary-flags-string? (parsed-package-query-interval-bounds query))
                    (parsed-package-query-interval-bounds query)
                    default-bounds)]
        [lo-variant (revision->revision-number #f (parsed-package-query-revision-min query))]
        [hi-variant (revision->revision-number #t (parsed-package-query-revision-max query))])
    (make-revision-interval
     (and lo-variant (coerce-revision-number lo-variant))
     (and hi-variant (coerce-revision-number hi-variant))
     #:lo-exclusive (boundary-flag->boolean (string-ref bounds 0))
     #:hi-exclusive (boundary-flag->boolean (string-ref bounds 1)))))


(define (make-exact-package-query provider name edition revision-number)
  (let ([rn (~a revision-number)])
    (parsed-package-query provider
                          name
                          edition
                          rn
                          rn
                          "ii")))


(module+ test
  (require rackunit)

  (test-equal? "Make exact query"
               (make-exact-package-query "acme" "anvil" "draft" 1)
               (parsed-package-query "acme" "anvil" "draft" "1" "1" "ii"))

  (test-case "Detect boundary flag strings"
    (check-true (boundary-flags-string? "ii"))
    (check-true (boundary-flags-string? "ie"))
    (check-true (boundary-flags-string? "ee"))
    (check-true (boundary-flags-string? "ei"))
    (check-false (boundary-flags-string? "e"))
    (check-false (boundary-flags-string? "i"))
    (check-false (boundary-flags-string? ""))
    (check-false (boundary-flags-string? "ab")))

  (test-equal? "Abbreviate queries"
               (abbreviate-exact-package-query (parsed-package-query "a" "b" "c" "0" "0" "ii"))
               "a:b:c:0")

  (test-case "Resolve non-numeric revision intervals"
    (define test-data
      '((97 100 "ii")
        (97 99  "ie")
        (98 100 "ei")
        (98 99  "ee")
        (97 100 "")))
    (for ([test-datum test-data])
      (match-define (list expected-lo expected-hi interval-flags) test-datum)
      (call-with-values (λ ()
                          (resolve-revision-interval
                           (parsed-package-query #f #f #f #\a #\d interval-flags)
                           (λ (_ r) (char->integer r))))
                        (λ (lo hi)
                          (test-equal? (format "Resolve non-numeric interval: ~a"
                                               (if (equal? interval-flags "")
                                                   "<empty-string>"
                                                   interval-flags))
                                       (cons lo hi)
                                       (cons expected-lo expected-hi))))))

  (test-case "Convert between queries and strings"
    (define (verify s expected) (check-equal? (format-parsed-package-query (parse-package-query s)) expected))
    (verify ""                "")
    (verify "a"               "a")
    (verify "a:b"             "a:b")
    (verify "a:b:c"           "a:b:c")
    (verify "a:b:c:d"         "a:b:c:d")
    (verify "a:b:c:d:e"       "a:b:c:d:e")
    (verify "a:b:c:d:e:f"     "a:b:c:d:e:f")
    (verify "a:b:c:d:e:f:g"   "a:b:c:d:e:f")
    (verify "a:b:c::e:f:g:h"  "a:b:c::e:f")))
