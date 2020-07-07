#lang racket/base

(provide make-endpoint
         call-with-each-catalog
         for-catalog
         (all-from-out net/url))

(require net/url
         "../config.rkt"
         "../logging.rkt")

(define current-zccatalog-service-endpoint
  (make-parameter (string->url (cdar (ZCPKG_SERVICE_ENDPOINTS)))))

(define (call-with-each-catalog f)
  (for/or ([base (in-list (ZCPKG_SERVICE_ENDPOINTS))])
    (<< "Trying ~v catalog at ~a" (car base) (cdr base))
    (parameterize ([current-zccatalog-service-endpoint (cdr base)])
      (f))))

(define-syntax-rule (for-catalog catalog-name expr ...)
  (parameterize ([current-zccatalog-service-endpoint
                  (string->url (cdr (assoc catalog-name (ZCPKG_SERVICE_ENDPOINTS))))])
    expr ...))

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
