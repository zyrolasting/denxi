#lang racket/base

(require racket/contract
         racket/function
         racket/match
         "format.rkt"
         "message.rkt"
         "path.rkt"
         "plugin.rkt"
         "query.rkt"
         "source.rkt"
         "string.rkt"
         "version.rkt")

(provide
 (except-out (struct-out catalog) catalog)
 (except-out (struct-out catalog-source) catalog-source)
 (contract-out
  [catalog
   (-> (-> string?)
       (-> string? string?)
       (-> string? string? string?)
       (-> string? string? string? string?)
       (-> string? string? string? string?)
       (-> string?)
       (-> string?
           string?
           string?
           string?
           (or/c #f revision-number-variant?))
       (-> resolved-package-query? (or/c #f source?))
       catalog?)]
  [catalog-source
   (-> package-query? catalog-source?)]
  [autocomplete-parsed-package-query
   (-> catalog?
       parsed-package-query?
       well-formed-package-query?)]
  [make-canonical-package-query
   (->* (catalog? parsed-package-query?)
        (#:force-complete-interval? any/c)
        (or/c #f resolved-package-query?))]
  [find-package-definition
   (-> catalog?
       package-query-variant?
       (or/c #f source?))]
  [set-catalog-filesystem-directory
   (-> catalog? complete-path? catalog?)]))


; Serves two purposes:
;   1. Autocompleting package queries
;   2. Finding package definitions
(struct catalog
  (get-default-provider
   get-default-package
   get-default-edition
   get-default-min-revision
   get-default-max-revision
   get-default-interval-bounds
   get-revision-number
   get-package-definition-source))


(define-source #:key (λ (s) (string-append ((catalog-get-default-provider (plugin-ref 'catalog default-catalog)))
                                           (catalog-source-query s)))
               (catalog-source [query package-query?])
  (let ([c (plugin-ref 'catalog default-catalog)])
    (with-handlers ([values %fail])
      (%fetch (find-package-definition c query) %fail))))


(define (autocomplete-parsed-package-query cat query)
  (match-define (catalog get-default-provider
                         get-default-package
                         get-default-edition
                         get-default-min-revision
                         get-default-max-revision
                         get-default-interval-bounds
                         _
                         _)
    cat)

  (define (get accessor get-default)
    (let ([v (accessor query)])
      (if (non-empty-string? v)
          v
          (get-default))))

  (define provider
    (get parsed-package-query-provider-name
         get-default-provider))
  (define package
    (get parsed-package-query-package-name
         (λ () (get-default-package provider))))
  (define edition
    (get parsed-package-query-edition-name
         (λ () (get-default-edition provider package))))
  (define revision-min
    (get parsed-package-query-revision-min
         (λ () (get-default-min-revision provider package edition))))
  (define revision-max
    (get parsed-package-query-revision-max
         (λ () (get-default-max-revision provider package edition))))
  (define interval-bounds
    (get parsed-package-query-interval-bounds
         get-default-interval-bounds))

  (parsed-package-query
   provider
   package
   edition
   revision-min
   revision-max
   interval-bounds))


(define (set-catalog-filesystem-directory cat directory)
  (define (build-catalog-path . els)
    (normalize-path (apply build-path directory (map ~a els))))

  ; normalize-path will resolve symlinsk
  (define/contract (get-revision-number provider package edition revision)
    (-> file-name-string? file-name-string? file-name-string? file-name-string? file-name-string?)
    (regexp-replace* #px"\\D"
                     (~a (file-name-from-path
                          (build-catalog-path provider package edition revision)))
                     ""))


  (define/contract (get-package-definition-source query)
    (-> resolved-package-query? (or/c #f source?))
    (match-define (parsed-package-query pr pk ed rl rh _) query)
    (define rln (string->number rl))
    (define rhn (string->number rh))
    (let loop ([index rhn])
      (define path (~a (build-catalog-path pr pk ed index) ".rkt"))
      (cond [(file-exists? path)
             (file-source (~a path))]
            [(< index rln)
             #f]
            [else (loop (sub1 index))])))

  (struct-copy catalog cat
               [get-revision-number
                get-revision-number]
               [get-package-definition-source
                get-package-definition-source]))

(define default-catalog
  (set-catalog-filesystem-directory
   (let ([get-default (const DEFAULT_STRING)])
     (catalog get-default
              get-default
              get-default
              (const "0")
              get-default
              (const "ii")
              #f
              #f))
   (build-path (find-system-path 'home-dir)
               "xiden-catalog")))


(define (make-canonical-package-query #:force-complete-interval? [force-complete-interval? #f]
                                      cat
                                      query)
  (define autocompleted-but-unresolved
    (autocomplete-parsed-package-query cat query))

  (define (revision->revision-number max? rev)
    ((catalog-get-revision-number cat)
     (parsed-package-query-provider-name autocompleted-but-unresolved)
     (parsed-package-query-package-name autocompleted-but-unresolved)
     (parsed-package-query-edition-name autocompleted-but-unresolved)
     rev))

  (define-values (lo hi)
    (resolve-revision-interval autocompleted-but-unresolved
                               revision->revision-number))

  (define (return a b)
    (struct-copy parsed-package-query autocompleted-but-unresolved
                 [revision-min (~a a)]
                 [revision-max (~a b)]
                 [interval-bounds "ii"]))

  (cond [(and (not hi) (not lo)) 'indeterminate]

        [(or (not lo) (not hi))
         (if force-complete-interval?
             (return (or lo hi) (or hi lo))
             'indeterminate)]

        [(< hi lo) 'backwards]

        [else (return lo hi)]))


(define (find-package-definition cat query)
  ((catalog-get-package-definition-source cat)
   (cond [(package-query? query)
          (make-canonical-package-query cat (parse-package-query query))]
         [(not (resolved-package-query? query))
          (make-canonical-package-query cat query)]
         [else query])))

(module+ test
  (require racket/format
           racket/function
           rackunit)

  (define test-catalog
    (catalog
     (const "provider")
     (const "package")
     (const "edition")
     (const "0")
     (const "100")
     (const "ie") ; <-- this shifts the later revisions back by one.
     (λ (pr pk ed rev)
       (if (revision-number-variant? rev)
           rev
           11))
     (λ (query)
       (match-define (parsed-package-query pr pk ed _ rv _) query)
       (file-source (build-path pr pk ed (~a rv))))))

  (define sources
    (list (find-package-definition test-catalog "")
          (find-package-definition test-catalog
                                   (parsed-package-query "acme" "anvil" "heavy" "9" "later" ""))
          (find-package-definition test-catalog "::heavy::6:")))

  (test-equal? "Find package definition sources using catalog"
               (map (compose ~a file-source-path) sources)
               '("provider/package/edition/99"
                 "acme/anvil/heavy/10"
                 "provider/package/heavy/5")))
