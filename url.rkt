#lang racket/base

; Extend net/url

(require net/url
         syntax/parse
         racket/contract)

(provide (all-from-out net/url)
         url-string
         (contract-out
          [url-string? predicate/c]
          [url-variant? predicate/c]
          [coerce-url (-> url-variant? url?)]
          [coerce-url-string (-> url-variant? url-string?)]))

(define (url-variant? v)
  (or (url? v)
      (url-string? v)))

(define (coerce-url-string v)
  (if (url? v)
      (url->string v)
      v))

(define (coerce-url v)
  (if (string? v)
      (string->url v)
      v))

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
    (check-pred url-variant? valid)
    (check-pred url-string? (coerce-url-string valid))
    (check-pred url? (coerce-url valid))
    (check-true (syntax-parse (datum->syntax #'whatever valid)
                  [v:url-string #t]
                  [_ #f]))))
