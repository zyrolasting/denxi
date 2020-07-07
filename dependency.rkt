#lang racket/base

; Define a data type representing a parsed version of a user's
; possibly vague and/or ambiguous request for a package.  Define the
; conditions in which a query is "concrete", meaning that it alone
; references an exact package with an exact version.


(provide (all-defined-out))

(require idiocket/contract
         idiocket/match
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

(define well-formed-dependency?
  (curry passes-invariant-assertion?
         (struct/c dependency
                   name-string?
                   name-string?
                   name-string?
                   boolean?
                   revision?
                   boolean?
                   revision?)))

(define concrete-dependency?
  (curry passes-invariant-assertion?
         (struct/c dependency
                   name-string?
                   name-string?
                   name-string?
                   boolean?
                   revision-number-string?
                   boolean?
                   revision-number-string?)))

(define (dependency-revision-min/for-inclusive-checks d)
  (add1-if (dependency-revision-min-exclusive? d)
           (dependency-revision-min d)))

(define (dependency-revision-max/for-inclusive-checks d)
  (sub1-if (dependency-revision-min-exclusive? d)
           (dependency-revision-min d)))

(define (dependency-match? to-match with-range)
  (and (concrete-dependency? to-match)
       (concrete-dependency? with-range)
       (andmap (λ (p) (equal? (p to-match) (p with-range)))
               (list dependency-provider-name
                     dependency-package-name
                     dependency-edition-name))
       (revision-range-subset? (dependency-revision-min/for-inclusive-checks to-match)
                               (dependency-revision-max/for-inclusive-checks to-match)
                               (dependency-revision-min/for-inclusive-checks with-range)
                               (dependency-revision-max/for-inclusive-checks with-range))))

(define (dependency-exact? d)
  (writeln d)
  (and (concrete-dependency? d)
       (not (dependency-revision-min-exclusive? d))
       (not (dependency-revision-max-exclusive? d))
       (equal? (dependency-revision-min d)
               (dependency-revision-max d))))

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
