#lang racket/base

(provide start-server)

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
         (prefix-in logged:
                    web-server/dispatchers/dispatch-log)
         (prefix-in lift:
                    web-server/dispatchers/dispatch-lift)
         "archiving.rkt"
         "config.rkt"
         "file.rkt"
         "format.rkt"
         "url.rkt"
         "verify.rkt"
         "workspace.rkt"
         "zcpkg-info.rkt"
         "zcpkg-query.rkt")

(define-syntax-rule (define-endpoint sig body ...)
  (define sig
    (with-handlers ([response? values])
      body ...)))

; The server uses the workspace for its files, like everything else.
(define (get-server-directory)
  (build-workspace-path "var/zcpkg"))

(define (build-server-path . args)
  (apply build-path (get-server-directory) args))

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
   [("file" (string-arg) ...) send-file]
   [("find" (string-arg) (string-arg) ...) search-packages]
   [("info" (string-arg)) send-info]
   [("artifact" (string-arg)) send-artifact]))


(define-endpoint (send-artifact req nss)
  (define query (nss->zcpkg-query nss))
  (for ([(candidate-path query-elements revno) (in-existing-revisions nss query)])
    (raise (response/file #:mime-type #"application/octet-stream"
                          (build-path candidate-path "archive.tgz")))))

(define-endpoint (send-info req nss)
  (define query (nss->zcpkg-query nss))
  (for ([(candidate-path query-elements revno) (in-existing-revisions nss query)])
    (raise (response/file #:mime-type #"text/plain"
                          (build-path candidate-path CONVENTIONAL_PACKAGE_INFO_FILE_NAME)))))

(define-endpoint (search-packages req nss rel-file-path-elements)
  (define query (nss->zcpkg-query nss))
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
  (define-values (minimum-revision-number maximum-revision-number)
    (resolve-revision-range query nss))
  (in-generator #:arity 3
   (for ([revno (in-range maximum-revision-number (sub1 minimum-revision-number) -1)])
     (define query-elements (deconstruct-zcpkg-query query revno))
     (define candidate-path (apply build-server-path query-elements))
     (when (directory-exists? candidate-path)
       (yield candidate-path query-elements revno)))
     (raise
      (cant-resolve #:code 404
                    (format "Cannot resolve ~s. There are no files in the requested range."
                            nss)))))

(define (deconstruct-zcpkg-query query revno)
  (list (zcpkg-query-provider-name query)
        (zcpkg-query-package-name query)
        (zcpkg-query-edition-name query)
        (~a revno)))


(define (resolve-revision-range query nss)
  (define lower-bound-info (get-interval-end-info query nss (zcpkg-query-revision-min query) "lower"))
  (define upper-bound-info (get-interval-end-info query nss (zcpkg-query-revision-max query) "upper"))
  (with-handlers ([exn:fail:zcpkg:invalid-revision-interval?
                   (λ (e)
                     (raise
                      (cant-resolve
                       #:code 400
                       (format "Can't resolve ~s. It corresponds to invalid number interval [~a, ~a]. "
                               (exn:fail:zcpkg:invalid-revision-interval-lo e)
                               (exn:fail:zcpkg:invalid-revision-interval-hi e))
                       (format "Did you mean ~s?"
                               (zcpkg-query->string
                                (struct-copy zcpkg-query query
                                             [revision-min (zcpkg-query-revision-max query)]
                                             [revision-max (zcpkg-query-revision-min query)]))))))])
    (get-inclusive-revision-range (zcpkg-query-revision-min-exclusive? query)
                                  (zcpkg-query-revision-max-exclusive? query)
                                  (zcpkg-info-revision-number lower-bound-info)
                                  (zcpkg-info-revision-number upper-bound-info))))


(define (get-interval-end-info query nss rev-name boundary-name)
  (define boundary-path (apply build-server-path (deconstruct-zcpkg-query query rev-name)))
  (unless (directory-exists? boundary-path)
    (raise (cant-resolve #:code 404
                           (format "Cannot resolve ~s. The ~a bound revision ~a does not exist in our records."
                                   nss
                                   boundary-name
                                   rev-name))))
  (read-zcpkg-info-from-directory boundary-path))


(define (nss->zcpkg-query nss)
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
    (coerce-zcpkg-query nss)))

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


(define (response/text #:code [code 200] fmt-string . a)
  (response/output #:code code
                   #:mime-type #"text/plain; charset=utf-8"
                   (λ (out) (apply fprintf out fmt-string a))))


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
