#lang racket/base

(provide clear-download-cache!
         download-info
         download-artifact
         current-url->response-values)

(require racket/list
         racket/path
         racket/port
         net/base64
         net/head
         "config.rkt"
         "zcpkg-query.rkt"
         "file.rkt"
         "format.rkt"
         "logging.rkt"
         "string.rkt"
         "url.rkt"
         "verify.rkt"
         "workspace.rkt"
         "zcpkg-info.rkt"
         "zcpkg-settings.rkt")


(define (get-cache-directory)
  (build-workspace-path "var/cache/zcpkg"))


(define (clear-download-cache!)
  (delete-directory/files (get-cache-directory)))


(define (zcpkg-query->url catalog-url path-prefix dep . others)
  (merge-urls
   (url #f #f #f #f #f
        (apply build-url-path
               path-prefix
               (zcpkg-query->string dep)
               others)
        null #f)
   catalog-url))


(define (download-info variant)
  (if (url? variant)
      (assert-valid-info variant (read-zcpkg-info (download-file variant)))
      (let* ([dep (coerce-zcpkg-query variant)]
             [dep-string (zcpkg-query->string dep)])
        (for/fold ([maybe-info #f])
                  ([name&string-url (in-list (ZCPKG_SERVICE_ENDPOINTS))])
          #:break maybe-info
          (define catalog-string-url (cdr name&string-url))
          (define catalog-url (string->url catalog-string-url))
          (download-info (zcpkg-query->url catalog-url "info" dep))))))


(define (assert-valid-info source-url info)
  (define errors (validate-zcpkg-info info))
  (unless (null? errors)
    (error 'download-info
           "Bad info from ~a:~n~a"
           (url->string source-url)
           (apply ~a (map (λ (err) (~a "  " err "\n"))))))
  info)


(define (download-artifact catalog-url dep info)
  (download-file (zcpkg-query->url catalog-url "artifact" dep)))


; Works if only the URL host and path matter.
(define (url->cache-path u)
  (build-path (get-cache-directory)
              (string-replace
               (bytes->string/utf-8
                (base64-encode (string->bytes/utf-8 (url->string u)) #""))
               "="
               "")))


(define (download-file u [cached-path (url->cache-path u)])
  (if (file-exists? cached-path)
      cached-path
      (begin
        (make-directory* (path-only cached-path))
        (let*-values ([(status headers in) ((current-url->response-values) u)])
          (and (= status 200)
               (call-with-output-file
                 #:exists 'truncate/replace
                 cached-path
                 (λ (out)
                   (copy-port in out)
                   cached-path)))))))

(define current-url->response-values
  (make-parameter
   (λ (u)
     (define-values (in headers)
       (get-pure-port/headers u
                              #:status? #t
                              #:redirections 3))
     (values (string->number (car (regexp-match #px"(\\d\\d\\d)" headers)))
             (for/hash ([pair (extract-all-fields headers)])
               (values (car pair)
                       (cdr pair)))
             in))))

(module+ test
  (require rackunit
           (submod "file.rkt" test))

  (test-workspace "Download a file"
      (define data #"chookie")
      (define (make-response u)
        (values 200 (hash) (open-input-bytes data)))

      (parameterize ([current-url->response-values make-response])
        (define u (string->url "https://example.com/blah/foo/blah"))
        (define cache-file (url->cache-path u))

        (test-false "Cached file does not yet exist"
                    (file-exists? cache-file))

        (define path (download-file u))

        (test-true "Cached file now exists"
                   (file-exists? cache-file))

        (test-equal? "Cached file has given data"
                     (file->bytes cache-file)
                     data)

        (test-equal? "Return path to cache file"
                     path
                     cache-file))))
