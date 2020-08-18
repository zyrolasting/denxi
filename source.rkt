#lang racket/base

; Define one data-driven way to fetch bytes from many sources.

(provide get-input-size
         open-input-source
         call-with-input-source
         call-with-first-source)

(require racket/function
         racket/port
         net/head
         net/url
         "rc.rkt"
         "mod.rkt")

; User may customize the means by which bytes are analyzed and fetched.
(define (get-get-source-info)
  (dynamic-require/mod 'get-source-info
                       (const get-source-info)))

(define (get-input-size key)
  ((get-get-source-info) 'size key))

(define (open-input-source key)
  ((get-get-source-info) 'port key))

(define (call-with-input-source src proc)
  (define in (open-input-source src))
  (dynamic-wind void
                (λ () (proc in))
                (λ () (close-input-port in))))

(define (call-with-first-source sources proc)
  (if (null? sources)
      'no-sources
      (call-with-input-source (car sources)
                              (λ (maybe-in)
                                (if maybe-in
                                    (proc maybe-in)
                                    (call-with-first-source (cdr sources)
                                                            proc))))))


(define (get-source-info/filesystem kind key)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (case kind
      [(size) (file-size key)]
      [(port) (open-input-file key)])))

(define (get-source-info/http kind key)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (case kind
      [(size)
       (define in (head-impure-port (string->url key)))
       (define headers (extract-all-fields (port->bytes in)))
       (string->number (bytes->string/utf-8 (extract-field #"content-length" headers)))]
      [(port) (get-pure-port #:redirections (XIDEN_DOWNLOAD_MAX_REDIRECTS)
                             (string->url key))])))

(define get-source-info
  (disjoin get-source-info/filesystem
           get-source-info/http))
