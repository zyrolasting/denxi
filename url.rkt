#lang racket/base

; Extend net/url

(require net/url
         "contract.rkt")

(provide (all-from-out net/url)
         (contract-out
          [url-string? predicate/c]))


(define (url-string? s)
  (with-handlers ([exn:fail? (λ _ #f)])
    (and (string->url s)
         #t)))


(module+ test
  (require rackunit)

  (define valid-url-strings
    '("foo"
      "https://example.com"
      "?a=b"))

  (for ([valid valid-url-strings])
    (check-pred url-string? valid)))
