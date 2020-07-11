#lang racket/base

(provide make-endpoint
         try-catalogs
         for-catalog
         current-zccatalog-service-endpoint
         (struct-out next-zcpkg-catalog))

(require "../url.rkt"
         "../config.rkt"
         "../logging.rkt")

(struct next-zcpkg-catalog ())

(define current-zccatalog-service-endpoint
  (make-parameter (string->url (cdar (ZCPKG_SERVICE_ENDPOINTS)))))

(define (try-catalogs f [catalogs (ZCPKG_SERVICE_ENDPOINTS)])
  (if (null? catalogs)
      (error "Tried all catalogs")
      (with-handlers ([next-zcpkg-catalog?
                       (λ (e) (try-catalogs f (cdr catalogs)))])
        (values (caar catalogs)
                (f (caar catalogs)
                   (string->url (cdar catalogs)))))))

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
