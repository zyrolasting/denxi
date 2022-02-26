#lang racket/base

(require racket/contract)
(provide (struct-out $http1.1:unsupported-scheme)
         (contract-out
          [http1.1-source-machine
           (->* ((or/c url? string?)
                 (or/c #f ssl-client-context?))
                (#:max-redirects exact-nonnegative-integer?
                 #:receive (-> exact-positive-integer?
                               (listof (cons/c string? string?))
                               (or/c exact-nonnegative-integer? +inf.0)
                               input-port?
                               source?))
                (machine/c source?))]))


(require racket/file
         racket/generic
         racket/match
         racket/path
         net/head
         net/url-connect
         net/url
         "file.rkt"
         "io.rkt"
         "machine.rkt"
         "message.rkt"
         "monad.rkt"
         "openssl.rkt"
         "port.rkt")


(define-message $http1.1:unsupported-scheme
  ([name string?]))

(define (http1.1-source-machine request-url-variant
                                client-identity
                                #:receive [make-receiver default-http1.1-receiver]
                                #:max-redirects [max-redirects 0])
  (machine
   (λ (state)
     (define request-url
       (if (string? request-url-variant)
           (string->url request-url-variant)
           request-url-variant))
     (define scheme
       (url-scheme request-url))
     (define derived-file-source
       (http/file-source request-url))
     (define network-machine
       (http1.1-invoke request-url
                       client-identity
                       max-redirects
                       make-receiver))
     (if (member scheme '("http" "https" "file"))
         (if derived-file-source
             (state-set-value state derived-file-source)
             (network-machine state))
         (state-halt-with state
                          ($http1.1:unsupported-scheme scheme))))))


(define (default-http1.1-receiver status-code headers content-length from-server)
  (prescribed-source content-length from-server))


(define (http1.1-invoke request-url id max-redirects make-source)
  (machine
   (λ (state)
     (define-values (from-server headers-string)
       (parameterize ([current-https-protocol (make-openssl-context id 'client)])
         (get-pure-port/headers
          #:redirections max-redirects
          #:method #"GET"
          #:status? #t
          request-url)))
     (define headers
       (extract-all-fields
        (if (bytes? headers-string)
            (bytes->string/utf-8 headers-string)
            headers-string)))
     (state-set-value state
                      (make-source (parse-status-code headers-string)
                                   headers
                                   (parse-content-length headers)
                                   from-server)))))


(define (http/file-source url)
  (and (equal? (url-scheme url) "file")
       (let ([path-els (map path/param-path (url-path url))])
         (file-source (apply build-path
                             (if (and (not (equal? (system-type 'os) 'windows))
                                      (url-path-absolute? url))
                                 (cons "/" path-els)
                                 path-els))))))


(define (parse-content-length headers)
  (define content-length-pair
    (assf (λ (el) (equal? (string-downcase el) "content-length"))
          headers))
  (if content-length-pair
      (string->number (or (cdr content-length-pair) "+inf.0"))
      +inf.0))


(define (parse-status-code headers-string)
  (string->number (car (regexp-match #px"\\d\\d\\d" headers-string))))


(module+ test
  (require racket/file
           racket/tcp
           racket/runtime-path
           rackunit
           mzlib/etc
           (submod "machine.rkt" test)
           "test.rkt")

  (define unsafe
    (ssl-make-client-context 'auto))

  (define (test-http-source url)
    (state-get-value ((http1.1-source-machine url unsafe))))

  (define (tap s)
    (mdo size := (source-measure s)
         port := (source-tap s)
         (machine-unit (λ () (values size port)))))

  (test file-fetch
        (define tmp-path (make-temporary-file))
        (with-handlers ([values
                         (λ (e)
                           (delete-file tmp-path)
                           (raise e))])
          (display-to-file #:exists 'truncate/replace "123" tmp-path)

          (define http-variant
            (test-http-source (format "file://~a" tmp-path)))

          (define file-variant
            (file-source tmp-path))

          (define (verify-output source)
            (call-with-values (state-get-value ((tap source)))
                              (λ (size port)
                                (compare equal? (port->bytes port) #"123")
                                (compare equal? size 3))))

          (verify-output file-variant)
          (verify-output http-variant)
          (delete-file tmp-path)))

  (test http1.1-fetch
    (define listener
      (tcp-listen 8018 1 #t))
    (define th
      (thread
       (λ ()
         (let loop ()
           (define-values (in out) (tcp-accept listener))
           (display "HTTP/1.1 200 OK\r\n" out)
           (display "https://www.example.com/\r\n" out)
           (display "Content-Type: text/html; charset=UTF-8\r\n" out)
           (display "Date: Fri, 28 Aug 2020 04:02:21 GMT\r\n" out)
           (display "Server: foo\r\n" out)
           (display "Content-Length: 5\r\n\r\n" out)
           (write-bytes #"1234567" out) ; send more than Content-Length claims
           (flush-output out)
           (close-output-port out)
           (close-input-port in)
           (loop)))))

    (define state
      ((mdo source := (http1.1-source-machine "http://127.0.0.1:8018" unsafe)
            (tap source))))

    (call-with-values (state-get-value state)
                      (λ (size port)
                        (compare equal? (port->bytes port) #"12345")
                        (compare equal? size 5)))
    (kill-thread th)
    (tcp-close listener)))
