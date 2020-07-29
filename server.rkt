#lang racket

(provide start-server)

(require racket/exn
         racket/runtime-path
         racket/sequence
         web-server/dispatch
         web-server/http
         web-server/web-server
         web-server/safety-limits
         (prefix-in seq:
                    web-server/dispatchers/dispatch-sequencer)
         (prefix-in logged:
                    web-server/dispatchers/dispatch-log)
         (prefix-in lift:
                    web-server/dispatchers/dispatch-lift)
         "archiving.rkt"
         "config.rkt"
         "zcpkg-query.rkt"
         "file.rkt"
         "url.rkt"
         "verify.rkt"
         "workspace.rkt"
         "zcpkg-info.rkt")


(define (get-server-directory)
  (build-workspace-path "var/zcpkg"))


(define (get-log-directory)
  (build-workspace-path "var/log/zcpkg"))


(define (zcpkg-info->public-file-path info)
  (build-path (get-server-directory)
              (zcpkg-info->relative-path info)))

(define (zcpkg-info->artifact-path info)
  (path-replace-extension (zcpkg-info->public-file-path info)
                          #".tgz"))



(define (start-server)
  (serve #:port 8080
         #:dispatch
         (seq:make
          (logged:make #:format logged:extended-format
                       #:log-path (build-path (get-log-directory) "server.log"))
          (lift:make promo-dispatcher)
          (lift:make not-found))
         #:safety-limits
         (make-safety-limits
          #:request-read-timeout (* 2 60)
          #:max-request-line-length 2048
          #:max-request-headers 12
          #:max-request-header-length 128
          #:max-request-body-length 64
          #:max-form-data-files 2
          #:max-form-data-file-length (* 10 1024 1024)
          #:form-data-file-memory-threshold (* 3 1024 1024))))


(define-values (promo-dispatcher promo-url promo-applies?)
  (dispatch-rules+applies
   [("provider" (string-arg)) send-provider]
   [((string-arg) "info") send-info]
   [((string-arg) "file") send-file]))


(define (send-provider req name)
  (define meta (build-path (get-server-directory) name "info.rktd"))
  (if (file-exists? meta)
      (response/file meta)
      (response/text #:code 404
                     "Provider ~s not found"
                     name)))


(define (send-info req urn)
  (cond [(not (zcpkg-query-string? urn))
         (response/text #:code 400
                        "~s is not a valid zcpkg-query string.~n"
                        urn)]

        [(equal? (request-method req) #"GET")
         (define info (sequence-ref (search-zcpkg-infos urn (in-installed-info)) 0))
         (response/output #:code 200
                          #:mime-type #"text/plain; charset=utf-8"
                          (λ (out)
                            (write-zcpkg-info info out)))]

        [(equal? (request-method req) #"PUT")
         (define data (request-post-data/raw req))

         ; Can info be written or replaced?
         ;  - Does artifact already exist with the info?
         ;  - Does the info have any errors?
         ;  - Does the info occupy the next available slot for the provider?
         (response/empty #:code 200)]

        [else (response/text #:code 400
                             "Invalid request")]))

(define (send-file req urn)
  (response/file (zcpkg-info->artifact-path (find-exactly-one-info urn))))

(define (not-found req)
  (response/output #:code 404
                   #:mime-type #"text/plain; charset=utf-8"
                   (λ (o) (displayln "Resource not found." o))))


(define (response/text #:code [code 200] fmt-string . a)
  (response/output #:code code
                   #:mime-type #"text/plain; charset=utf-8"
                   (λ (out) (apply fprintf out fmt-string a))))


(define (response/file path)
  (response/output #:code 200
                   #:mime-type #"application/octet-stream"
                   (λ (out)
                     (call-with-input-file path
                       (λ (in) (copy-port in out))))))


(module+ test
  (require rackunit)

  (define (url->request u)
    (make-request #"GET" (string->url u) null
                  (delay null) #f "1.2.3.4" 80 "4.3.2.1")))
