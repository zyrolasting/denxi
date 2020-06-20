#lang racket/base

(provide BASE-SERVICE-ENDPOINT
         make-endpoint
         current-zccatalog-service-endpoint)

(require net/url)

; Leverage the envvar and parameter for testing purposes.
(define BASE-SERVICE-ENDPOINT
  (string->url (or (getenv "GSYS_ZCP_CATALOG_ENDPOINT")
                   "https://zcpkgs.com:443")))

(define current-zccatalog-service-endpoint
  (make-parameter BASE-SERVICE-ENDPOINT))


(define-syntax-rule (make-endpoint expr ...)
  (struct-copy url (current-zccatalog-service-endpoint) expr ...))

; TODO: Should preference be given to b?
(define (merge-endpoints a b)
  (define (or+ p) (or (p a) (p b)))

  (apply url (map or+ (list url-scheme
                            url-user
                            url-host
                            url-port
                            url-path-absolute?
                            url-path
                            url-query
                            url-fragment))))
