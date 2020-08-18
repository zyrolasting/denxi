#lang racket/base

(provide start-server
         get-server-directory)

(require racket/exn
         racket/generator
         racket/path
         racket/port
         racket/runtime-path
         racket/sequence
         racket/string
         web-server/dispatch
         web-server/http
         web-server/web-server
         web-server/safety-limits
         web-server/dispatchers/dispatch
         xml
         (prefix-in seq:
                    web-server/dispatchers/dispatch-sequencer)
         (prefix-in lift:
                    web-server/dispatchers/dispatch-lift)
         "archiving.rkt"
         "config.rkt"
         "file.rkt"
         "format.rkt"
         "input-info.rkt"
         "package-info.rkt"
         "printer.rkt"
         "query.rkt"
         "url.rkt"
         "openssl.rkt"
         "workspace.rkt"
         "xiden-messages.rkt")

(define-syntax-rule (define-endpoint sig body ...)
  (define sig
    (with-handlers ([response? values])
      body ...)))

; The server uses the workspace for its files, like everything else.
(define (get-server-directory)
  (build-workspace-path "var/xiden"))

(define (build-server-path . args)
  (apply build-path (get-server-directory) args))

(define (log-request req)
  (write-output
   ($on-request (request-method req)
                (request-client-ip req)
                (request-host-ip req)
                (let ([R (headers-assq*
                          #"Referer"
                          (request-headers/raw req))])
                  (and R (header-value R)))
                (url->string (request-uri req))
                (current-seconds)))
  (next-dispatcher))

(define (start-server #:port port)
  (serve #:port port
         #:dispatch
         (seq:make
          (lift:make log-request)
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
   [("file" (string-arg) ...) send-file]
   [("find" (string-arg) (string-arg) ...) search-packages]))

(define-endpoint (search-packages req nss rel-file-path-elements)
  (define query (nss->xiden-query nss))
  (for ([(candidate-path query-elements revno) (in-existing-revisions nss query)])
    (raise
     (response/output
      #:code 303
      #:mime-type #f
      #:headers
      (list (header #"Location"
                    (string->bytes/utf-8
                     (url->string
                      (struct-copy url (request-uri req)
                                   [path (apply build-url-path
                                                "file"
                                                (append query-elements
                                                        rel-file-path-elements))])))))
      void))))

(define (in-existing-revisions nss query)
  ; TODO: Database query
  (void))

(define (nss->xiden-query nss)
  (with-handlers
    ([exn:fail?
      (λ (e)
        (raise
         (cant-resolve
          #:code 400
          (format "~s seems to have invalid syntax. The correct syntax has one of these forms: "
                  nss)
          '(ul (li (code "<provider>:<package>:<edition>:<revision>")
                   ", e.g. "
                   (code "example.com:widget:for-teachers:5") " or "
                   (code "example.com:widget:for-teachers:with-acknowledgements"))
               (li (code "<provider>:<package>:<edition>:<min-revision>:<max-revision>")
                   ", e.g. "
                   (code "example.com:widget:for-students:8:10"))
               (li (code "<provider>:<package>:<edition>:<i|e>:<min-revision>:<i|e>:<max-revision>")
                   ", e.g. "
                   (code "example.com:widget:for-msu:i:start-peer-review:e:for-publication"))))))])
    (coerce-xiden-query nss)))

(define (cant-resolve #:code code . msg)
  (response/html5 #:code code
                  (html-doc/minimal "Not found"
                                    `(p . ,msg))))

(define (html-doc head . body)
  `(html (head . ,head)
         (body . ,body)))

(define (html-doc/minimal title . body)
  (apply html-doc `((title ,title))
         body))

(define (response/html5 #:code [code 200] xexpr)
  (response/output #:code code #:mime-type #"text/html"
   (λ (o)
     (displayln "<!DOCTYPE html>" o)
     (displayln (xexpr->string xexpr) o))))

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
        [(and (equal? (build-path "index.html") (file-name-from-path complete-path))
              (directory-exists? (path-only complete-path)))
         (response/html5
          (html-doc `((title ,"Listing"))
                    `(ul . ,(map (λ (rel-path)
                                   (define href (path->string rel-path))
                                   `(li (a ((href ,href)) ,href)))
                                 (directory-list (path-only complete-path))))))]
        [else (next-dispatcher)]))


(define (not-found req)
  (response/output #:code 404
                   #:mime-type #"text/plain; charset=utf-8"
                   (λ (o) (displayln "Resource not found." o))))


(define (response/file #:mime-type mime-type path)
  (response/output #:code 200
                   #:mime-type mime-type
                   #:headers
                   (list (header #"Content-Disposition"
                                 (string->bytes/utf-8
                                  (format "attachment; filename=\"~a\""
                                          (file-name-from-path path)))))
                   (λ (out)
                     (call-with-input-file path
                       (λ (in) (copy-port in out))))))


(module+ test
  (require rackunit
           racket/promise)

  (define (url->request u)
    (make-request #"GET" (string->url u) null
                  (delay null) #f "1.2.3.4" 80 "4.3.2.1")))
