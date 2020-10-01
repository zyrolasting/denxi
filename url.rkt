#lang racket/base

(require racket/function
         racket/list
         net/url
         "contract.rkt")

(provide (all-from-out net/url)
         (contract-out
          [build-url-path
           (->* ()
                #:rest (non-empty-listof (or/c path/param? path-string?))
                (non-empty-listof path/param?))]
          [indicates-fs-path? (-> url? boolean?)]
          [url->maybe-path (->* (url?) (path-string?) (or/c #f path?))]
          [url-string? predicate/c]
          [merge-urls (-> url? url? url?)]))


(define (build-url-path . els)
  (map (λ (el)
         (cond [(path/param? el) el]
               [(string? el) (path/param el null)]
               [(path? el) (path/param (path->string el) null)]))
       els))


(define (url-string? s)
  (with-handlers ([exn:fail? (λ _ #f)])
    (and (string->url s)
         #t)))

(define (url->maybe-path u [relative-path-root (current-directory)])
  (with-handlers ([exn? (const #f)])
    (simplify-path
     (apply build-path
            (if (and (url-path-absolute? u)
                     (not (member (get-leading-path-element u) '(same up))))
                (car (filesystem-root-list))
                relative-path-root)
            (filter-map (λ (pp)
                          (and (not (equal? "" (path/param-path pp)))
                               (path/param-path pp)))
                        (url-path u))))))

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


; TODO: Should preference be given to b?
(define (merge-urls a b)
  (define (or+ p) (or (p a) (p b)))

  (apply url (map or+ (list url-scheme
                            url-user
                            url-host
                            url-port
                            url-path-absolute?
                            url-path
                            url-query
                            url-fragment))))

(module+ test
  (require rackunit)

  (check-equal? (get-leading-path-element (string->url "https://example.com/blah;uh/foo?a=b#baz"))
                "blah")

  (test-equal? "Build a URL path"
               (apply build-url-path "a"
                      (path/param "b" null)
                      (build-path "c")
                      '("d" "e" "f"))
               (list (path/param "a" null)
                     (path/param "b" null)
                     (path/param "c" null)
                     (path/param "d" null)
                     (path/param "e" null)
                     (path/param "f" null))))
