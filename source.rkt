#lang racket/base

; Given a list of strings, find the scope of work for an installation.

(require racket/contract)

(provide (all-defined-out))

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


(define (source->variant v [requesting-path (current-directory)])
  (or (source->maybe-path v requesting-path)
      (source->maybe-dependency v)
      (string->url v)))

(define (variant->source v)
  (cond [(url? v) (url->string v)]
        [(dependency? v) (dependency->string v)]
        [(path? v) (path->string v)]))

(define (source->maybe-path v [relative-path-root (current-directory)])
  (cond [(path? v)
         (and (directory-exists? v)
              (simplify-path (if (complete-path? v) v
                                 (build-path relative-path-root v))))]

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


(module+ test
  (require rackunit
           racket/runtime-path)

  (define-runtime-path ./ ".")

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
