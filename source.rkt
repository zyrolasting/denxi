#lang racket/base

; Define methods for fetching bytes from some origin when given only a
; string.

(require "contract.rkt")
(provide
 (contract-out
  [fetch-source
   (-> string?
       (-> input-port?
           (or/c +inf.0 exact-positive-integer?)
           any/c)
       any/c)]
  [transfer-package-info
   (-> input-port?
       (or/c +inf.0 exact-positive-integer?)
       ($with-messages/c package-info?))]))


(require racket/function
         net/head
         "config.rkt"
         "exn.rkt"
         "file.rkt"
         "message.rkt"
         "mod.rkt"
         "package-info.rkt"
         "path.rkt"
         "port.rkt"
         "query.rkt"
         "rc.rkt"
         "url.rkt")

(define+provide-message $source (user-string))
(define+provide-message $source-method-ruled-out $source (reason))
(define+provide-message $source-fetched $source ())
(define+provide-message $source-unfetched $source ())

(define (mibibytes->bytes mib)
  (inexact->exact (ceiling (* mib 1024 1024))))


(define (fetch-source source request-transfer)
  (define (mod-fallback . _) (const #f))

  ; The fetch procedures can just return #f if they cannot
  ; find something. This instruments the procedures so that
  ; they can be composed in fetch-source.
  (define (lift-fetch-source-method f method-name)
    (λ (status)
      (if status
          (:return status)
          (with-handlers
            ([exn:fail?
              (λ (e)
                (attach-message #f ($source-method-ruled-out source (exn->string e))))])
            (let ([maybe-result (f source request-transfer)])
              (if maybe-result
                  (:return maybe-result)
                  (attach-message
                   #f
                   ($source-method-ruled-out source
                                             (format "Method produced nothing: ~a"
                                                     method-name)))))))))

  (:do #:with (:return (fetch-source/filesystem source request-transfer))
       (lift-fetch-source-method fetch-source/http "HTTP")
       (lift-fetch-source-method fetch-source/xiden-query "Package query")
       (lift-fetch-source-method (load-plugin 'fetch-source mod-fallback mod-fallback) "Plugin")
       (λ (maybe-path)
         (attach-message maybe-path
                         (if maybe-path
                             ($source-fetched source)
                             ($source-unfetched source))))))


(define (fetch-source/filesystem source make-file)
  (and (file-exists? source)
       (make-file (open-input-file source)
                  (+ (* 20 1024) ; for Mac OS resource forks
                     (file-size source)))))


(define (fetch-source/http source make-file)
  (define in (head-impure-port (string->url source)))
  (define headers (extract-all-fields (port->string in)))
  (define content-length-pair (assf (λ (el) (equal? (string-downcase el) "content-length")) headers))
  (define est-size
    (if content-length-pair
        (string->number (or (cdr content-length-pair) "+inf.0"))
        "+inf.0"))
  (make-file (get-pure-port #:redirections (XIDEN_DOWNLOAD_MAX_REDIRECTS) (string->url source))
             est-size))


(define (fetch-source/xiden-query source make-file)
  (define pkginfo
    (for/or ([u (in-list (map/service-endpoints source (XIDEN_SERVICE_ENDPOINTS)))])
      (read-package-info (get-pure-port u #:redirections (XIDEN_DOWNLOAD_MAX_REDIRECTS)))))
  (and pkginfo
       (let-values ([(i o) (make-pipe)])
         (write-config #:pretty? #t (package-info->hash pkginfo) null o)
         (flush-output o)
         (close-output-port o)
         (make-file i (mibibytes->bytes (XIDEN_FETCH_PKGDEF_SIZE_MB))))))


(define (transfer-package-info from-source est-size)
  (define max-size (mibibytes->bytes (XIDEN_FETCH_PKGDEF_SIZE_MB)))
  (define-values (from-pipe to-pipe) (make-pipe max-size))
  (define transfer-output
    (transfer from-source to-pipe
              #:transfer-name "package-info"
              #:max-size max-size
              #:buffer-size (mibibytes->bytes (max (/ (XIDEN_FETCH_PKGDEF_SIZE_MB) 5) 5))
              #:timeout-ms (XIDEN_FETCH_TIMEOUT_MS)
              #:est-size est-size))
  (close-output-port to-pipe)
  (:merge transfer-output
          (:return (read-package-info from-pipe))))


(module+ test
  (require racket/file
           racket/tcp
           rackunit
           "setting.rkt")

  (define plugin-module-datum
    '(module mods racket/base
       (provide fetch-source)
       (define (fetch-source source make-file)
         (make-file (open-input-string source)
                    (string-length source)))))

  (define mod-source "====[{]::[}]====")

  (define (try-mod-fetch)
    (fetch-source mod-source
                  (λ (in est-size)
                    (check-equal? est-size (string-length mod-source))
                    (check-equal? (read-string est-size in) mod-source))))

  (call-with-temporary-file
   (λ (tmp)
     (write-to-file plugin-module-datum tmp #:exists 'truncate/replace)
     (parameterize ([(setting-derived-parameter XIDEN_MODS_MODULE) tmp])
       (define source (path->string tmp))
       (test-equal? "Fetch from file"
                    (fetch-source
                     source
                     (λ (in est-size)
                       (test-equal? "Can read file" (read in) plugin-module-datum)
                       (test-true "Can estimate file size" (>= est-size (file-size tmp)))
                       'some-value))
                    (attach-message 'some-value ($source-fetched source)))

       (test-equal? "Fetch from mod"
                    (find-message $source-fetched? (try-mod-fetch))
                    ($source-fetched mod-source)))))

  ; Notice we just left the parameterize that set the mod path
  (test-case "Investigate fetch failures"
    (define fetch-output (try-mod-fetch))
    (test-equal? "Fetch fails when mod is not available"
                 (find-message $source-unfetched? fetch-output)
                 ($source-unfetched mod-source))
    (check-false (null? (filter $source-method-ruled-out? ($with-messages-accumulated fetch-output)))))

  (test-case "Fetch over HTTP"
    (define listener (tcp-listen 8018 1 #t))
    (define th
      (thread
       (λ ()
         (let loop ([num 0])
           (define-values (in out) (tcp-accept listener))
           (display "HTTP/1.1 200 OK\r\n" out)
           (display "https: //www.example.com/\r\n" out)
           (display "Content-Type: text/html; charset=UTF-8\r\n" out)
           (display "Date: Fri, 28 Aug 2020 04:02:21 GMT\r\n" out)
           (display "Server: foo\r\n" out)
           (display "Content-Length: 5\r\n\r\n" out)
           (unless (= num 0)
             (display "12345" out))
           (flush-output out)
           (close-output-port out)
           (close-input-port in)
           (loop (add1 num))))))

    (define source "http://127.0.0.1:8018")
    (check-equal?
     (fetch-source source
                   (λ (in est-size)
                     (check-eq? est-size 5)
                     (check-equal? (read-bytes est-size in) #"12345")
                     (kill-thread th)
                     (tcp-close listener)))
     (attach-message (void)
                     ($source-fetched source)))))
