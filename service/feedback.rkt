#lang racket/base

(provide FEEDBACK-CHAR-LIMIT
         feedback-dispatcher
         feedback-endpoint)

(require racket/cmdline
         racket/date
         racket/file
         racket/format
         racket/port
         racket/runtime-path
         net/url
         web-server/http
         (prefix-in limit: web-server/dispatchers/limit)
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         "endpoint.rkt")

; Server enforces char limit
(define FEEDBACK-CHAR-LIMIT 2000)

; Assume worst-case of UTF-8 encoded string taking 4 bytes per character.
(define BYTE-LIMIT (* FEEDBACK-CHAR-LIMIT 4))

(define-runtime-path feedback/ "submitted-feedback")
(make-directory* feedback/)

(define (response/text #:code [code 200] proc)
  (response/output #:code code
                   #:mime-type #"text/plain; charset=utf-8"
                   (λ (o) (parameterize ([current-output-port o])
                            (proc)))))

(define (make-feedback-path)
  (build-path feedback/ (make-feedback-file-name)))

(define (make-datetime-string)
  (define d (current-date))
  (format "~a-~a-~a"
          (date-year d)
          (date-month d)
          (date-day d)))

(define (make-feedback-file-name [suffix #f])
  (define fn (string-append (make-datetime-string)
                            (if suffix
                                (~a suffix)
                                "")))
  (if (file-exists? fn)
      (make-feedback-file-name (if suffix (add1 suffix) 0))
      fn))

(define (accept-feedback req)
  (call-with-output-file #:exists 'append
    (make-feedback-path)
    (λ (o)
      (copy-port (open-input-bytes (request-post-data/raw req)) o)
      (display "\n=============\n\n" o)))
  (response/text (λ () (displayln "Got the message. Thanks!"))))

(define feedback-dispatcher
  (filter:make #rx"/feedback"
               (limit:make 5
                           (lift:make accept-feedback))))

(define feedback-endpoint
  (make-endpoint [path (list (path/param "feedback" null))]))
