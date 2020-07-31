#lang racket

(provide start-server)

(require racket/exn
         racket/runtime-path
         racket/sequence
         web-server/dispatch
         web-server/http
         web-server/web-server
         web-server/safety-limits
         web-server/dispatchers/dispatch
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
  (make-directory* (get-log-directory))
  (serve #:port 8080
         #:dispatch
         (seq:make
          (logged:make #:format logged:extended-format
                       #:log-path (build-path (get-log-directory) "server.log"))
          (lift:make service-dispatcher)
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


(define-values (service-dispatcher service-url service-applies?)
  (dispatch-rules+applies
   [("file" (string-arg) ...) send-file]))

(define (send-file req contains-relative-path-elements)
  (define relative-path-elements
    (filter non-empty-string?
            contains-relative-path-elements))

  (define relative-path
    (apply build-path
           (if (null? relative-path-elements)
               '(".")
               relative-path-elements)))

  (define complete-path (build-path (get-server-directory) relative-path))

  (cond [(directory-exists? complete-path)
         (define u (request-uri req))
         (define normalized-path
           (filter (λ (pp) (non-empty-string? (path/param-path pp)))
                   (url-path u)))

         (redirect-to (url->string (struct-copy url u
                                                [path (append normalized-path
                                                              (list (path/param "index.html" null)))]))
                      permanently)]
        [(file-exists? complete-path)
         (response/file #:mime-type
                        (case (path-get-extension complete-path)
                          [(#".html") #"text/html"]
                          [else #"application/octect-stream"])
                        complete-path)]
        [else (next-dispatcher)]))


(define (not-found req)
  (response/output #:code 404
                   #:mime-type #"text/plain; charset=utf-8"
                   (λ (o) (displayln "Resource not found." o))))


(define (response/text #:code [code 200] fmt-string . a)
  (response/output #:code code
                   #:mime-type #"text/plain; charset=utf-8"
                   (λ (out) (apply fprintf out fmt-string a))))


(define (response/file #:mime-type mime-type path)
  (response/output #:code 200
                   #:mime-type mime-type
                   (λ (out)
                     (call-with-input-file path
                       (λ (in) (copy-port in out))))))


(module+ test
  (require rackunit)

  (define (url->request u)
    (make-request #"GET" (string->url u) null
                  (delay null) #f "1.2.3.4" 80 "4.3.2.1")))
