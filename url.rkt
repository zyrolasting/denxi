#lang racket/base

(require racket/function
         racket/list
         net/url
         "contract.rkt")

(provide (all-from-out net/url)
         (contract-out
          [indicates-fs-path? (-> url? boolean?)]
          [url->maybe-path (->* (url?) (path-string?) (or/c #f path?))]
          [url-string? predicate/c]
          [merge-urls (-> url? url? url?)]))

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
