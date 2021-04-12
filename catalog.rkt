#lang racket/base

(require racket/exn
         racket/function
         racket/match
         racket/promise
         "codec.rkt"
         "format.rkt"
         "input-info.rkt"
         "integrity.rkt"
         "l10n.rkt"
         "localstate.rkt"
         "logged.rkt"
         "openssl.rkt"
         "package.rkt"
         "path.rkt"
         "plugin.rkt"
         "query.rkt"
         "signature.rkt"
         "source.rkt"
         "strict-rc.rkt"
         "string.rkt"
         "url.rkt"
         "version.rkt"
         "workspace.rkt")

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
       (-> resolved-package-query? (or/c #f md-algorithm/c))
       (-> resolved-package-query? (or/c #f source?))
       (-> resolved-package-query? (or/c #f source?))
       (-> resolved-package-query? (or/c #f source?))
       catalog?)]
  [catalog-source
   (-> package-query-variant?
       catalog-source?)]
  [get-default-catalog (-> catalog?)]
  [add-catalogged-inputs
   (-> catalog? package? (listof package-query-variant?) package?)]
  [autocomplete-parsed-package-query
   (-> catalog?
       parsed-package-query?
       well-formed-package-query?)]
  [make-canonical-package-query
   (->* (catalog? parsed-package-query?)
        (#:force-complete-interval? any/c)
        (or/c #f resolved-package-query?))]
  [find-input-info
   (->* (catalog?
         package-query-variant?)
        (string?)
        input-info?)]
  [find-integrity-info
   (-> catalog?
       package-query-variant?
       integrity-info?)]
  [find-signature-info
   (-> catalog?
       package-query-variant?
       signature-info?)]
  [find-package-definition
   (-> catalog?
       package-query-variant?
       (or/c #f source?))]
  [find-package-definition-digest
   (-> catalog?
       package-query-variant?
       (or/c #f source?))]
  [find-package-definition-signature
   (-> catalog?
       package-query-variant?
       (or/c #f source?))]
  [find-package-definition-public-key
   (-> catalog?
       package-query-variant?
       (or/c #f source?))]
  [find-package-definition-chf
   (-> catalog?
       package-query-variant?
       md-algorithm/c)]
  [set-catalog-filesystem-directory
   (-> catalog?
       complete-path?
       catalog?)]
  [set-catalog-http-host
   (-> catalog?
       (or/c url? url-string?)
       catalog?)]))


(struct catalog
  (get-default-provider
   get-default-package
   get-default-edition
   get-default-min-revision
   get-default-max-revision
   get-default-interval-bounds
   get-revision-number
   get-package-definition-source
   get-package-definition-chf
   get-package-definition-digest-source
   get-package-definition-public-key-source
   get-package-definition-signature-source))


(define (get-default-catalog)
  (set-catalog-http-host
   (let ([get-default (const DEFAULT_STRING)])
     (catalog get-default
              get-default
              get-default
              (const "0")
              get-default
              (const "ii")
              #f
              #f
              (const DEFAULT_CHF)
              #f
              #f
              #f))
   (rc-ref 'XIDEN_DEFAULT_CATALOG_BASE_URL)))

(define-source #:key catalog-source-query (catalog-source [query package-query-variant?])
  (define input
    (find-input-info (plugin-ref 'canonical-catalog (get-default-catalog))
                     query))

  (writeln (url->string (http-source-request-url (integrity-info-digest (input-info-integrity input)))))

  (define value
    (run+print-log (fetch-exact-input/cached input)))

  (if (eq? value FAILURE)
      (%fail (void))
      (%fetch (file-source (build-workspace-path (path-record-path value))))))


(define (autocomplete-parsed-package-query cat query)
  (match-define (catalog get-default-provider
                         get-default-package
                         get-default-edition
                         get-default-min-revision
                         get-default-max-revision
                         get-default-interval-bounds
                         _ _ _ _ _ _)
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
    (simplify-path (apply build-path directory (map ~a els))))

  ; normalize-path will resolve symlinsk
  (define/contract (get-revision-number provider package edition revision)
    (-> file-name-string?
        file-name-string?
        file-name-string?
        file-name-string?
        (or/c #f file-name-string?))

    (define path (build-catalog-path provider package edition revision))
    (and (file-exists? path)
         (regexp-replace* #px"\\D"
                          (~a (file-name-from-path (normalize-path path)))
                          "")))

  (define/contract (get-source query . suffix)
    (->* (resolved-package-query?) () #:rest list? (or/c #f source?))
    (match-define (parsed-package-query pr pk ed rl rh _) query)
    (define rln (string->number rl))
    (define rhn (string->number rh))
    (let loop ([index rhn])
      (define path (build-catalog-path pr pk ed index))
      (cond [(file-exists? path)
             (file-source (apply ~a path suffix))]
            [(< index rln)
             #f]
            [else (loop (sub1 index))])))

  (struct-copy catalog cat
               [get-revision-number
                get-revision-number]
               [get-package-definition-source
                (λ (q) (get-source q))]
               [get-package-definition-digest-source
                (λ (q) (get-source q "." ((catalog-get-package-definition-chf cat) q)))]
               [get-package-definition-public-key-source
                (λ (q) (file-source
                        (build-catalog-path
                         (parsed-package-query-provider-name q)
                         ".public-key")))]
               [get-package-definition-signature-source
                (λ (q) (get-source q "." ((catalog-get-package-definition-chf cat) q) ".sig"))]))


(define (set-catalog-http-host cat base-url-variant)
  (define make-http-source
    (let ([base-url (coerce-url base-url-variant)])
      (λ path-elements
        (http-source
         (struct-copy url base-url
                      [path
                       (append (url-path base-url)
                               (map (λ (e) (path/param e null))
                                    path-elements))])))))

  (define (make-definition-relative-source resolved-query . file-suffix)
    (make-http-source (parsed-package-query-provider-name resolved-query)
                      (parsed-package-query-package-name resolved-query)
                      (parsed-package-query-edition-name resolved-query)
                      (apply ~a file-suffix)))

  (define/contract (get-revision-number provider package edition revision)
    (-> string? string? string? string? (or/c #f string?))
    (fetch (make-http-source provider package edition revision)
           (λ (in est-size)
             (define rx-match (regexp-match #px"\\(revision-number (\\d+)\\)" in))
             (close-input-port in)
             (and rx-match
                  (coerce-string (cadr rx-match))))
           raise))

  (define/contract (get-package-definition-source resolved-query)
    (-> resolved-package-query? (or/c #f source?))
    (make-definition-relative-source resolved-query
                                     (parsed-package-query-revision-max resolved-query)))


  (define/contract (get-package-definition-digest-source resolved-query)
    (-> resolved-package-query? (or/c #f source?))
    (make-definition-relative-source resolved-query
                                     (parsed-package-query-revision-max resolved-query)
                                     "."
                                     ((catalog-get-package-definition-chf cat)
                                      resolved-query)))

  (define/contract (get-package-definition-public-key-source resolved-query)
    (-> resolved-package-query? (or/c #f source?))
    (make-http-source (parsed-package-query-provider-name resolved-query)
                      "public-key"))

  (define/contract (get-package-definition-signature-source resolved-query)
    (-> resolved-package-query? (or/c #f source?))
    (make-definition-relative-source resolved-query
                                     (parsed-package-query-revision-max resolved-query)
                                     "."
                                     ((catalog-get-package-definition-chf cat)
                                      resolved-query)
                                     ".sig"))

  (struct-copy catalog cat
               [get-revision-number
                get-revision-number]
               [get-package-definition-source
                get-package-definition-source]
               [get-package-definition-digest-source
                get-package-definition-digest-source]
               [get-package-definition-public-key-source
                get-package-definition-public-key-source]
               [get-package-definition-signature-source
                get-package-definition-signature-source]))



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


(define (add-catalogged-inputs cat pkg queries)
  (struct-copy package pkg
               [inputs
                (append (package-inputs pkg)
                        (map (λ (q) (find-input-info cat q))
                             queries))]))


(define (find-input-info cat query [name (string-replace query ":" "_")])
  (input-info name
              (find-package-definition cat query)
              (find-integrity-info cat query)
              (find-signature-info cat query)))


(define (find-integrity-info cat query)
  (integrity-info (find-package-definition-chf cat query)
                  (find-package-definition-digest cat query)))


(define (find-signature-info cat query)
  (signature-info (find-package-definition-public-key cat query)
                  (find-package-definition-signature cat query)))


(define (find-package-definition cat query)
  (find-item catalog-get-package-definition-source cat query))


(define (find-package-definition-digest cat query)
  (find-item catalog-get-package-definition-digest-source cat query))


(define (find-package-definition-signature cat query)
  (find-item catalog-get-package-definition-signature-source cat query))


(define (find-package-definition-public-key cat query)
  (find-item catalog-get-package-definition-public-key-source cat query))


(define (find-package-definition-chf cat query)
  (find-item catalog-get-package-definition-chf cat query))


(define (find-item acc cat query)
  ((acc cat)
   (canonicalize-package-query-variant cat query)))


(define (canonicalize-package-query-variant cat query)
  (cond [(package-query? query)
         (make-canonical-package-query cat (parse-package-query query))]
        [(not (resolved-package-query? query))
         (make-canonical-package-query cat query)]
        [else query]))



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
       (file-source (build-path pr pk ed (~a rv))))
     #f
     #f
     #f
     #f))

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
