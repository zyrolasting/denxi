#lang racket/base

; Extend net/url

(require net/url
         syntax/parse
         "contract.rkt")

(provide (all-from-out net/url)
         url-string
         (contract-out
          [url-string? predicate/c]))


(define (url-string? s)
  (with-handlers ([exn:fail? (λ _ #f)])
    (and (string->url s)
         #t)))

(define-syntax-class url-string
  (pattern (~var str string)
           #:when (url-string? (syntax-e #'str))))

(module+ test
  (require rackunit)

  (define valid-url-strings
    '("foo"
      "https://example.com"
      "?a=b"))

  (for ([valid valid-url-strings])
    (check-pred url-string? valid)
    (check-true (syntax-parse (datum->syntax #'whatever valid)
                  [v:url-string #t]
                  [_ #f]))))
