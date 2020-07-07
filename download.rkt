#lang racket/base


(provide download-artifact-info
         download-artifact
         clear-download-cache!)


(require racket/file
         racket/list
         racket/path
         racket/port
         file/cache
         net/url
         net/head
         "logging.rkt"
         "workspace.rkt"
         "config.rkt"
         "verify.rkt"
         "zcpkg-info.rkt"
         "service/endpoint.rkt"
         "dependency.rkt")


(define (get-cache-directory)
  (build-workspace-path "var/cache/zcpkg"))

; Warning: Firewalls may remove headers not mentioned in RFC 2616.
; Provide workaround with tailored GET?
(define (download-artifact-info dep)
  (define-values (catalog-name maybe-well-formed-artifact-info)
    (call-with-each-catalog
     (λ ()
       (define info-url (dependency->url dep))
       (define from-service (head-impure-port info-url))
       (read-zcpkg-info from-service))))

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


(define (download-file u path)
  (cache-file
   #:exists-ok? #t
   #:notify-cache-use (λ (s) (<< "Cache hit for ~a" s))
   #:log-error-string (λ (s) (<< s))
   #:log-debug-string (λ (s) (<< #:level 'debug s))
   path path (get-cache-directory)
   (λ ()
     (make-directory* (path-only path))
     (call-with-output-file path
       (λ (out)
         (copy-port (get-pure-port u #:redirections (ZCPKG_DOWNLOAD_MAX_REDIRECTS)) out))))))


(define (clear-download-cache!)
  (delete-directory/files (get-cache-directory)))
