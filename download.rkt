#lang racket/base

(provide download-info
         download-artifact
         clear-download-cache!)

(require racket/file
         racket/list
         racket/path
         racket/port
         net/url
         "verify.rkt"
         "service/endpoint.rkt"
         "source/dependency-query.rkt")

(define cache-directory
  (make-parameter (build-path (find-system-path 'addon-dir)
                              "zcpkg-download-cache")))

(define (make-artifact-info-url project distributor artifact edition revision-min revision-max)
  (make-endpoint
   [path
    (list (path/param project (list distributor)))
          (path/param artifact (list edition
                                     revision-min
                                     revision-max))]))

(define (download-info query)
  (define artifact-info-url (dependency-query->artifact-info-url query))
  (define artifact-info-path (download-file artifact-info-url))
  (read-artifact-info artifact-info-path))


(define (download-artifact info)
  (define artifact-info-url (artifact-info->artifact-url info))
  (define artifact-path (download-file str))
  (define checksum (artifact-info-integrity info))
  (unless (equal? (make-digest artifact-path) checksum)
    (error 'download-artifact
           "Integrity violation for ~a" artifact-path))
  artifact-path)


(define (download-file pathstr)
  (define u (if (url? pathstr) pathstr (string->url pathstr)))
  (define path
    (path-replace-extension
     (build-path (cache-directory) (make-digest u))
     (path-get-extension
      (path/param-path (last (url-path u))))))
  (if (file-exists? path)
      (begin
        (printf "cache hit for ~a~n" path)
        path)
      (begin
        (make-directory* (cache-directory))
        (call-with-output-file path
          (λ (out)
            (copy-port (get-pure-port u #:redirections 3) out)
            path)))))


(define (clear-download-cache!)
  (delete-directory/files (cache-directory)))
