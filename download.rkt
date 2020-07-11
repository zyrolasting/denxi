#lang racket/base

(provide clear-download-cache!)

(require racket/file
         racket/list
         racket/path
         racket/port
         net/url
         "logging.rkt"
         "workspace.rkt"
         "config.rkt"
         "verify.rkt"
         "zcpkg-info.rkt"
         "service/endpoint.rkt"
         "dependency.rkt")


(define (get-cache-directory)
  (build-workspace-path "var/cache/zcpkg"))

(define (download/dependency dep)
  (define-values (catalog-name maybe-well-formed-artifact-info)
    (try-catalogs
     (λ (catalog-name catalog-url-base)
       (define info-url (dependency->url dep))
       (read-zcpkg-info (download-file info-url
                                       catalog-name
                                       #f)))))

  (unless catalog-name
    (error 'download-artifact-info
           "~v: Artifact info not found in any catalog"
           dep))

  (unless (well-formed-zcpkg-info? maybe-well-formed-artifact-info)
    (error 'download-artifact-info
           "~v: Ill-formed artifact metadata from catalog ~v"
           dep
           catalog-name))

  (values catalog-name
          maybe-well-formed-artifact-info))


(define (download-artifact catalog-name query info)
  (define artifact-info-url (for-catalog catalog-name (dependency->url query)))
  (define artifact-path (download-file artifact-info-url))
  (define checksum (zcpkg-info-integrity info))
  (unless (equal? (make-digest artifact-path) checksum)
    (error 'download-artifact
           "Integrity violation for ~a" artifact-path))
  artifact-path)


(define (download-file/with-cache u)
  ; TODO: Some URLs will map to the same path here,
  ; causing unwanted cache hits. Map URLs to unique
  ; file paths.
  (define cached-path
    (build-path (get-cache-directory)
                (apply build-path
                       (url-host u)
                       (map path/param-path (url-path u)))))
  (if (file-exists? cached-path)
      cached-path
      (download-file u cached-path)))


(define (download-file u path)
  (make-directory* (path-only path))
  (define-values (in headers)
    (get-pure-port/headers u
                           #:status? #t
                           #:redirections
                           (ZCPKG_DOWNLOAD_MAX_REDIRECTS)))

  (define status (string->number (car (regexp-match #px"(\\d\\d\\d)" headers))))

  (and (= status 200)
       (call-with-output-file
         #:exists 'truncate/replace
         path
         (λ (out)
           (copy-port in out)
           path))))


(define (clear-download-cache!)
  (delete-directory/files (get-cache-directory)))
