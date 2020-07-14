#lang racket

(provide start-server)

(require racket/exn
         racket/runtime-path
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
         "dependency.rkt"
         "workspace.rkt"
         "config.rkt"
         "url.rkt")

(define (start-server)
  (serve #:port 8080
         #:dispatch
         (seq:make
          (logged:make #:format logged:extended-format #:log-path "server.log")
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
   [("") landing-page]
   [("info") #:method (or "get" "put") send/recv-info]
   [("artifact") #:method (or "get" "put") send/recv-artifact]))


(define (response/text #:code [code 200] fmt-string . a)
  (response/output #:code code
                   #:mime-type #"text/plain; charset=utf-8"
                   (λ (out) (apply fprintf out fmt-string a))))

(define (landing-page req)
  (response/xexpr
   `(html (head (title "zcpkg"))
          (body (h1 "Zero-Collection Racket Package Index")
                (p "This is a prototype registry for the "
                   (a ((href "https://github.com/zyrolasting/zcpkg")) "zcpkg")
                   " package manager.")))))


(define (send/recv-info req urn)
  (cond [(not (dependency-string? urn))
         (response/text #:code 400
                        "~s is not a valid dependency string.~n"
                        urn)]

        [(equal? (request-method req) "get")
         (define info.rkt "")
         (response/output #:code 200
                          #:mime-type #"text/plain; charset=utf-8"
                          (λ (out)
                            (call-with-input-file info.rkt
                              (λ (in) (copy-port in out)))))]

        [(equal? (request-method req) "put")
         (define data (request-post-data/raw req))


         ; Can info be written or replaced?
         ;  - Does artifact already exist with the info?
         ;  - Does the info have any errors?
         ;  - Does the info occupy the next available slot for the provider?
         (response/empty #:code 200)]))

(define (send/recv-artifact req urn)
  (define package.tgz "")
  (response/output #:code 200
                   #:mime-type #"application/octet-stream"
                   (λ (out)
                     (call-with-input-file package.tgz
                       (λ (in) (copy-port in out))))))


(define (not-found req)
  (response/output #:code 404
                   #:mime-type #"text/plain; charset=utf-8"
                   (λ (o) (displayln "Resource not found." o))))

(module+ test
  (require rackunit)

  (define (url->request u)
    (make-request #"GET" (string->url u) null
                  (delay null) #f "1.2.3.4" 80 "4.3.2.1")))
