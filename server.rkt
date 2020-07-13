#lang racket

(provide start-server)

(require racket/exn
         racket/runtime-path
         web-server/dispatch
         web-server/http
         web-server/web-server
         (prefix-in seq:
                    web-server/dispatchers/dispatch-sequencer)
         (prefix-in logged:
                    web-server/dispatchers/dispatch-log)
         (prefix-in lift:
                    web-server/dispatchers/dispatch-lift)
         "workspace.rkt"
         "config.rkt"
         "url.rkt")

(define (not-found req)
  (response/output #:code 404
                   #:mime-type #"text/plain; charset=utf-8"
                   (λ (o) (displayln "Resource not found." o))))


(define (start-server)
  (serve #:port 8080
         #:dispatch
         (seq:make
          (logged:make #:format logged:extended-format #:log-path "server.log")
          (lift:make promo-dispatcher)
          (lift:make not-found))))


(define-values (promo-dispatcher promo-url promo-applies?)
  (dispatch-rules+applies
   [("") landing-page]
   [("info") #:method (or "get" "put") send/recv-info]
   [("artifact") #:method (or "get" "put") send/recv-artifact]))


(define (url->request u)
  (make-request #"GET" (string->url u) null
                  (delay null) #f "1.2.3.4" 80 "4.3.2.1"))


(define landing/xexpr
  `(html (head (title "zcpkg"))
         (body (h1 "Zero-Collection Racket Package Index"))))

(define (landing-page req)
  (response/xexpr landing/xexpr))

(define (send/recv-info req)
  ; Does URL have a full dependency URN?

  (case (request-method req)
    [("get")
     (define info.rkt "")
     (response/output #:code 200
                      #:mime-type #"text/plain; charset=utf-8"
                      (λ (out)
                        (call-with-input-file info.rkt
                          (λ (in) (copy-port in out)))))]
    [("put")
     ; Can info be written or replaced?
     ;  - Does artifact already exist with the info?
     ;  - Does the info have any errors?
     ;  - Does the info occupy the next available slot for the provider?
     (response/empty #:code 200)]))


(define (send/recv-artifact req)
  (define package.tgz "")
  (response/output #:code 200
                   #:mime-type #"application/octet-stream"
                   (λ (out)
                     (call-with-input-file package.tgz
                       (λ (in) (copy-port in out))))))
