#lang racket/base

; Given a list of strings, find the scope of work for an installation.

(require racket/contract)

(provide find-scope-of-work)

(require racket/path
         racket/set
         "config.rkt"
         "dependency.rkt"
         "download.rkt"
         "file.rkt"
         "message.rkt"
         "string.rkt"
         "url.rkt"
         "zcpkg-info.rkt")


(define (source->variant v requesting-path)
  (or (source->maybe-path v requesting-path)
      (source->maybe-dependency v)
      (string->url v)))

(define (variant->source v)
  (cond [(url? v) (url->string v)]
        [(dependency? v) (dependency->string v)]
        [(path? v) (path->string v)]))

(define (source->maybe-path v [relative-path-root (current-directory)])
  (cond [(path? v)
         (define pkg-path
           (simplify-path (if (complete-path? v) v
                              (build-path relative-path-root v))))

         (if (directory-exists? pkg-path) pkg-path
             (raise (exn:fail:user
                     (format (~a "Package not found: ~a~n~n"
                                 "Assuming your command is correct, a package~n"
                                 "may have an incorrect relative path as a dependency.~n~n"
                                 "Base path: ~a~n"
                                 "Relative path: ~a~n")
                             pkg-path
                             relative-path-root
                             (find-relative-path relative-path-root pkg-path))
                     (current-continuation-marks))))]

        [(url? v)
         (and (or (not (url-scheme v))
                  (equal? (url-scheme v) "file"))
              (source->maybe-path (url->maybe-path v relative-path-root)
                                  relative-path-root))]

        [(url-string? v)
         (source->maybe-path (string->url v)
                             relative-path-root)]

        [else #f]))


(define (source->maybe-dependency v)
  (with-handlers ([exn? (λ _ #f)])
    (define dep (coerce-dependency v))
    (and (well-formed-dependency? dep)
         dep)))


(define (resolve-source-iter id info requesting-directory seen)
  (unless (hash-has-key? seen id)
    (hash-set! seen id info)
    (for ([dependency-source (in-list (zcpkg-info-dependencies info))])
      (resolve-source dependency-source
                      requesting-directory
                      seen))))

(define (resolve-source source requesting-directory seen)
  (define variant (source->variant source requesting-directory))
  (if (path? variant)
      (resolve-source-iter (path->directory-path (simplify-path variant))
                           (read-zcpkg-info-from-directory variant)
                           variant
                           seen)
      (resolve-source-iter source
                           (download-info variant)
                           requesting-directory
                           seen)))


(define (find-scope-of-work package-sources)
  (define seen (make-hash))
  (for ([source (in-list package-sources)])
    (resolve-source source (current-directory) seen))
  seen)



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

   (define foo-info (copy-zcpkg-info dummy-zcpkg-info [dependencies '("." "../bar")]))
   (define bar-info (copy-zcpkg-info dummy-zcpkg-info [dependencies '("../foo" "acme:anvil:heavy:0")]))
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
     (check-equal? (make-hash
                    `((,(build-path* "foo") . ,foo-info)
                      (,(build-path* "bar") . ,bar-info)
                      ("acme:anvil:heavy:0" . ,acme-info)))
                   (find-scope-of-work '("./foo" "./bar")))))

  (parameterize ([current-directory ./])
    (test-false "If it looks like a file path, then it doesn't count"
                (source->maybe-path "./source.rkt"))

    (test-equal? "If it looks like a directory path, then use it."
                 (source->maybe-path ".")
                 (simplify-path "."))

    (test-equal? "Mimic rules for file:// url pointing to a file."
                 (source->maybe-path "file:///./source.rkt")
                 (source->maybe-path "./source.rkt"))

    (test-equal? "Mimic rules for file:// url pointing to a directory."
                 (source->maybe-path "file:///.")
                 (source->maybe-path "."))

    (test-false "Don't make paths out of things that don't exist."
                (source->maybe-path "weeee.erk")))

  (test-true "All valid dependency strings are valid sources"
             (andmap (λ (s) (dependency? (source->maybe-dependency s)))
                     (list "provider:package"
                           "provider:package:draft"
                           "provider:package:draft:newest"
                           "provider:package:draft:oldest:newest"
                           "provider:package:draft:i:oldest:e:newest")))

  (test-false "Invalid dependency strings are not valid sources"
              (dependency? (source->maybe-dependency "fsdfhbsdfj"))))
