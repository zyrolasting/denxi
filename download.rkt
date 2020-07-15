#lang racket/base

(provide clear-download-cache!
         download-info
         download-artifact)

(require racket/list
         racket/path
         racket/port
         "config.rkt"
         "dependency.rkt"
         "file.rkt"
         "logging.rkt"
         "string.rkt"
         "url.rkt"
         "verify.rkt"
         "workspace.rkt"
         "zcpkg-info.rkt")


(define (get-cache-directory)
  (build-workspace-path "var/cache/zcpkg"))


(define (clear-download-cache!)
  (delete-directory/files (get-cache-directory)))


(define (dependency->url catalog-url path-prefix dep)
  (merge-urls
   (url #f #f #f #f #f
        (list (path/param path-prefix null)
              (path/param (dependency->string dep) null))
        null #f)
   catalog-url))


(define (download-info variant)
  (if (url? variant)
      (values variant (assert-valid-info (read-zcpkg-info (download-file variant))))
      (let* ([dep (coerce-dependency variant)]
             [dep-string (dependency->string dep)])
        (for/fold ([u #f] [p #f])
                  ([name&string-url (in-list (ZCPKG_SERVICE_ENDPOINTS))])
          #:break p
          (define catalog-string-url (cdr name&string-url))
          (define catalog-url (string->url catalog-string-url))
          (download-info (dependency->url catalog-url "info" dep))))))


(define (assert-valid-info source-url info)
  (define errors (validate-zcpkg-info info))
  (unless (null? errors)
    (error 'download-info
           "Bad info from ~a:~n~a"
           (url->string source-url)
           (apply ~a (map (λ (err) (~a "  " err "\n"))))))
  info)


(define (download-artifact catalog-url dep info)
  (download-file (dependency->url catalog-url "artifact" dep)))


; Works if only the URL host and path matter.
(define (url->cache-path u)
  (build-path (get-cache-directory)
              (apply build-path
                     (url-host u)
                     (map path/param-path (url-path u)))))


(define (download-file u [cached-path (url->cache-path u)])
  (if (file-exists? cached-path)
      (begin
        (printf "cache: ~a~n" (url->string u))
        cached-path)
      (begin
        (printf "download: ~a~n" (url->string u))
        (make-directory* (path-only cached-path))
        (let*-values ([(in headers)
                       (get-pure-port/headers u
                                              #:status? #t
                                              #:redirections
                                              (ZCPKG_DOWNLOAD_MAX_REDIRECTS))]

                      [(status)
                       (string->number (car (regexp-match #px"(\\d\\d\\d)" headers)))])
          (and (= status 200)
               (call-with-output-file
                 #:exists 'truncate/replace
                 cached-path
                 (λ (out)
                   (copy-port in out)
                   cached-path)))))))
