#lang racket/base

(require racket/function
         racket/list
         net/url
         "contract.rkt")

(provide (all-from-out net/url)
         (contract-out
          [indicates-fs-path? (-> url? boolean?)]
          [url-string? predicate/c]))


(define (url-string? s)
  (with-handlers ([exn:fail? (λ _ #f)])
    (and (string->url s)
         #t)))

(define (indicates-fs-path? u)
  (define leading (get-leading-path-element u))
  (or (eq? 'up leading)
      (eq? 'same leading)
      (equal? "" leading) ; For UNC path (Windows)
      (url-path-absolute? u)))


(define (get-leading-path-element u)
  (define path/params (url-path u))
  (if (null? path/params)
      (raise-argument-error 'get-leading-path-element "URL with a path" u)
      (path/param-path (car path/params))))


(module+ test
  (require rackunit)

  (check-equal? (get-leading-path-element (string->url "https://example.com/blah;uh/foo?a=b#baz"))
                "blah"))
