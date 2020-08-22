#lang racket/base

; Define the many ways users can express an input,
; then resolve those expressions to actual inputs.

(provide user-string->package-info)

(require racket/match
         racket/path
         racket/port
         racket/set
         racket/sequence
         version/utils
         net/head
         "config.rkt"
         "contract.rkt"
         "encode.rkt"
         "file.rkt"
         "format.rkt"
         "input-info.rkt"
         "input-forms-lang.rkt"
         "integrity.rkt"
         "message.rkt"
         "package-info.rkt"
         "query.rkt"
         "racket-version.rkt"
         "rc.rkt"
         "signature.rkt"
         "string.rkt"
         "url.rkt"
         "openssl.rkt"
         "workspace.rkt"
         "xiden-messages.rkt")


(define input-forms-namespace
  (module->namespace "input-forms-lang.rkt"))


(define (user-string->package-info str)
  (read-package-info
   (XIDEN_TRUST_BAD_DIGEST
    #t
    (λ ()
      (fulfill-input (input-info "package-def" (list str) #f #f))))))


(define (make-input-info variant)
  ((select-input-variant-converter variant) variant))


(define (select-input-variant-converter variant)
  (cond [(string? variant) string->input-info]
        [(url? variant) url->input-info]
        [(list? variant) list->input-info]
        [(path? variant) path->input-info]
        [(input-info? variant) values]
        [(xiden-query? variant) xiden-query->input-info]))


(define (xiden-query->input-info v)
  (define str (xiden-query->string v))
  (input-info 'package
              (encode 'base32 str)
              (map (λ (url-string)
                     (define u (string->url url-string))
                     (url->string
                      (struct-copy url u
                                   [path
                                    (append (url-path u)
                                            (list (path/param str null)))])))
                   (XIDEN_SERVICE_ENDPOINTS))
              #f
              #f))


(define (string->input-info v)
  (cond [(xiden-query-string? v)
         (make-input-info (string->xiden-query v))]
        [(or (file-exists? v) (directory-exists? v))
         (make-input-info (string->path v))]
        [(url-string? v)
         (make-input-info (string->url v))]
        [(list? (with-handlers ([exn:fail:read? (λ _ #f)])
                  (read (open-input-string v))))
         (make-input-info (read (open-input-string v)))]))


(define (list->input-info v)
  (eval v input-forms-namespace))


(define (path->input-info v)
  (input-info 'file
              (path->string (file-name-from-path v))
              (list v)
              (integrity-info 'sha384 (make-digest v 'sha384))
              #f))


; A URL can still refer to a local file.
; Account for that here.
(define (url->input-info v)
  (if (or (not (url-scheme v))
          (equal? (url-scheme v) "file"))
      (make-input-info (url->maybe-path v))
      (input-info 'file
                  (encode 'base32 (url->string v))
                  (list (url->string v))
                  #f
                  #f)))

(module+ test
  (require rackunit)

  (XIDEN_SERVICE_ENDPOINTS (list "https://foo.example.com"
                                 "https://bar.example.com"
                                 "https://zap.example.com")
    (λ ()
      (test-case "Expand query to input definition"
        (let* ([query (coerce-xiden-query "example.com:package")]
               [expanded-query-string (xiden-query->string query)])
        (test-equal? "Expand abbreviated query"
                     (xiden-query->input-info query)
                     (input-info 'package
                                 (encode 'base32 expanded-query-string)
                                 (map (λ (pref) (string-append pref "/" expanded-query-string))
                                      (XIDEN_SERVICE_ENDPOINTS))
                                 #f
                                 #f)))))))
