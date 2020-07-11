#lang racket/base

; Define a package source as a string that could refer to a local file
; or a remote resource.  A package source should transform to an URL.

(require racket/contract)

(provide
 (contract-out
  [resolve-source
   (-> string? (or/c url? dependency? path?))]))

(require "config.rkt"
         "dependency.rkt"
         "string.rkt"
         "url.rkt")

(define (source->maybe-path v [relative-path-root (current-directory)])
  (cond [(path? v)
         (define p
           (cond [(directory-exists? v)
                  (build-path v "info.rkt")]
                 [(file-exists? v)
                  (build-path v)]
                 [else #f]))
         (and p
              (simplify-path (if (complete-path? p) p
                                 (build-path relative-path-root p))))]

        [(url? v)
         (and (or (not (url-scheme v))
                  (equal? (url-scheme v) "file"))
              (source->maybe-path (url->maybe-path v relative-path-root)))]

        [(url-string? v)
         (source->maybe-path (string->url v))]

        [else #f]))

(define (source->maybe-dependency v)
  (with-handlers ([exn? (λ _ #f)])
    (define dep (coerce-dependency v))
    (and (well-formed-dependency? dep)
         dep)))

(define (resolve-source v)
  (or (source->maybe-path v)
      (source->maybe-dependency v)
      (string->url v)))

(module+ test
  (require rackunit
           racket/runtime-path)

  (define-runtime-path ./ ".")

  (parameterize ([current-directory ./])
    (test-equal? "If it looks like a file path, then make it a file path"
                 (source->maybe-path "./source.rkt")
                 (simplify-path "./source.rkt"))

    (test-equal? "If it looks like a directory path, then make it a info.rkt file path"
                 (source->maybe-path ".")
                 (simplify-path "./info.rkt"))

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
                     (list "provider/package"
                           "provider/package;draft"
                           "provider/package;draft;newest"
                           "provider/package;draft;oldest;newest"
                           "provider/package;draft;i;oldest;e;newest")))

  (test-false "Invalid dependency strings are not valid sources"
              (dependency? (source->maybe-dependency "fsdfhbsdfj"))))
