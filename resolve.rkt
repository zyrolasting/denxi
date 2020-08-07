#lang racket/base

; Given a list of strings, find the scope of work for an installation.

(require racket/contract)

(provide find-scope-of-work
         source->variant)

(require (only-in racket/list remove-duplicates)
         racket/path
         racket/set
         racket/sequence
         "config.rkt"
         "zcpkg-query.rkt"
         "download.rkt"
         "file.rkt"
         "format.rkt"
         "message.rkt"
         "string.rkt"
         "url.rkt"
         "zcpkg-info.rkt")


(define (source->variant v requesting-path)
  (or (source->maybe-path v requesting-path)
      (source->maybe-zcpkg-query v)
      (string->url v)))

(define (variant->source v)
  (cond [(url? v) (url->string v)]
        [(zcpkg-query? v) (zcpkg-query->string v)]
        [(path? v) (path->string v)]))

(define (source->maybe-path #:must-exist? [must-exist? #t] v [relative-path-root (current-directory)])
  (cond [(path? v)
         (define pkg-path
           (simplify-path (if (complete-path? v) v
                              (build-path relative-path-root v))))

         (if (directory-exists? pkg-path) pkg-path
             (and must-exist?
                  (raise-user-error
                   (format (~a "Package not found: ~a~n~n"
                               "Assuming your command is correct, a package~n"
                               "may have an incorrect relative path as a dependency.~n~n"
                               "Base path: ~a~n"
                               "Relative path: ~a~n")
                           pkg-path
                           relative-path-root
                           (find-relative-path relative-path-root pkg-path)))))]

        [(url? v)
         (and (or (not (url-scheme v))
                  (equal? (url-scheme v) "file"))
              (source->maybe-path #:must-exist? must-exist?
                                  (url->maybe-path v relative-path-root)
                                  relative-path-root))]

        [(url-string? v)
         (source->maybe-path #:must-exist? must-exist?
                             (string->url v)
                             relative-path-root)]

        [else #f]))


(define (source->maybe-zcpkg-query v)
  (with-handlers ([exn? (λ _ #f)])
    (define dep (coerce-zcpkg-query v))
    (and (well-formed-zcpkg-query? dep)
         dep)))


(define (resolve-source-iter id info requesting-directory seen)
  (unless (hash-has-key? seen id)
    (hash-set! seen id #f)
    (hash-set! seen id
               (cons info
                     (for/list ([dependency-source (in-list (zcpkg-info-dependencies info))])
                       (resolve-source dependency-source
                                       requesting-directory
                                       seen)))))
  info)

(define (resolve-source source requesting-directory seen)
  (define variant (source->variant source requesting-directory))
  (if (path? variant)
      (resolve-source-iter (path->directory-path (simplify-path variant))
                           (read-zcpkg-info-from-directory variant)
                           variant
                           seen)
      (resolve-source-iter source
                           (find-info variant)
                           requesting-directory
                           seen)))

(define (find-info variant)
  (with-handlers ([exn:fail? (λ (e) (download-info variant))])
    (sequence-ref (search-zcpkg-infos variant (in-installed-info))
                  0)))

(define (find-scope-of-work package-sources)
  (define seen (make-hash))
  (for ([source (in-list package-sources)])
    (resolve-source source (current-directory) seen))

  (for/hash ([(k v) (in-hash seen)])
    (values k (remove-duplicates v))))


(module+ test
  (require rackunit
           racket/runtime-path
           (submod "file.rkt" test)
           (submod "zcpkg-info.rkt" test))

  (define-runtime-path ./ ".")

  (define (build-path* el)
    (path->directory-path (build-path (current-directory) el)))

  (test-workspace "Find scope of work for a given package source"
    ; To make it interesting: Give every package a dependency cycle.

   (define foo-info
     (copy-zcpkg-info dummy-zcpkg-info
                      [provider-name "fooby"]
                      [package-name "foo"]
                      [dependencies '("." "../bar")]))
   (define bar-info
     (copy-zcpkg-info dummy-zcpkg-info
                      [provider-name "barry"]
                      [package-name "bar"]
                      [dependencies '("../foo" "acme:anvil:heavy:0")]))
   (define acme-info
     (copy-zcpkg-info dummy-zcpkg-info
                      [provider-name "acme"]
                      [package-name "anvil"]
                      [edition-name "heavy"]
                      [dependencies '("acme:anvil:heavy:0")]
                      [integrity #"blah"]))

   (write-zcpkg-info-to-directory foo-info "foo")
   (write-zcpkg-info-to-directory bar-info "bar")

   (define mock-remote-info
     (let ([buffer (open-output-bytes)])
       (write-zcpkg-info acme-info buffer)
       (get-output-bytes buffer #t)))

   (parameterize ([current-url->response-values
                   (λ (u) (values 200 (hash) (open-input-bytes mock-remote-info)))])
     (define h (find-scope-of-work '("./foo" "./bar")))
     (check-equal? (hash-ref h (build-path* "foo")) (list foo-info bar-info))
     (check-equal? (hash-ref h (build-path* "bar")) (list bar-info foo-info acme-info))
     (check-equal? (hash-ref h "acme:anvil:heavy:0") (list acme-info))))

  (parameterize ([current-directory ./])
    (test-false "If it looks like a file path, then it doesn't count"
                (source->maybe-path #:must-exist? #f "./source.rkt"))

    (test-equal? "If it looks like a directory path, then use it."
                 (source->maybe-path ".")
                 (simplify-path "."))

    (test-equal? "Mimic rules for file:// url pointing to a file."
                 (source->maybe-path #:must-exist? #f "file:///./source.rkt")
                 (source->maybe-path #:must-exist? #f "./source.rkt"))

    (test-equal? "Mimic rules for file:// url pointing to a directory."
                 (source->maybe-path #:must-exist? #f "file:///.")
                 (source->maybe-path #:must-exist? #f "."))

    (test-false "Don't make paths out of things that don't exist."
                (source->maybe-path #:must-exist? #f "weeee.erk")))

  (test-true "All valid zcpkg-query strings are valid sources"
             (andmap (λ (s) (zcpkg-query? (source->maybe-zcpkg-query s)))
                     (list "provider:package"
                           "provider:package:draft"
                           "provider:package:draft:newest"
                           "provider:package:draft:oldest:newest"
                           "provider:package:draft:i:oldest:e:newest")))

  (test-false "Invalid zcpkg-query strings are not valid sources"
              (zcpkg-query? (source->maybe-zcpkg-query "fsdfhbsdfj"))))
