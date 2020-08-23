#lang racket/base

; Define a package input format and related operations. The main two
; being to declare a package input, and to download the correct bytes
; for that input.

(require racket/contract)
(provide (struct-out input-info)
         (contract-out
          [well-formed-input-info/c
           flat-contract?]
          [fulfill-input
           (-> well-formed-input-info/c
               (-> string? real? any)
               complete-path?)]))


(require racket/function
         racket/list
         racket/port
         racket/string
         net/url
         net/head
         "encode.rkt"
         "exn.rkt"
         "file.rkt"
         "integrity.rkt"
         "mod.rkt"
         "output.rkt"
         "path.rkt"
         "rc.rkt"
         "signature.rkt"
         "workspace.rkt")

(struct input-info
  (name       ; The name of the file to hold the input bytes
   sources    ; Where to look to get the input bytes
   integrity  ; Integrity information
   signature) ; Signature for authentication
  #:prefab)


(define well-formed-input-info/c
  (struct/c input-info
            non-empty-string?
            (non-empty-listof any/c)
            (or/c (λ (info) (XIDEN_TRUST_BAD_DIGEST))
                  well-formed-integrity-info/c)
            (or/c (λ (info) (or (XIDEN_TRUST_BAD_DIGEST)
                                (XIDEN_TRUST_UNSIGNED)))
                  well-formed-signature-info/c)))


(define (fetch-source input source on-progress)
  (define expected-size (get-input-size source))
  (call-with-input-source
   source
   (λ (in)
     (define tmp (make-temporary-file))
     (with-handlers ([values (λ (e) (delete-file* tmp) (raise e))])
       (copy-source-port in (input-info-name input) source expected-size tmp on-progress)
       (raise-unless-postconditions-met input source tmp)
       (define path (get-object-path (make-digest tmp (integrity-info-algorithm (input-info-integrity input)))))
       (make-directory* (path-only path))
       (rename-file-or-directory tmp path)
       path))))


(define (fulfill-input input on-progress)
  (for/fold ([path (get-maybe-existing-object-path input)]
             [errors null])
            ([source (in-list (input-info-sources input))])
    (with-handlers ([exn:fail:xiden:source? (λ (e) (values #f (cons e errors)))])
      (values (or path (fetch-source input source on-progress))
              (reverse errors)))))


(define (get-maybe-existing-object-path input)
  (with-handlers ([exn? (const #f)])
    (define path
      (get-object-path
       (integrity-info-digest
        (input-info-integrity input))))
    (and (file-exists? path) path)))


(define (raise-unless-postconditions-met input source tmp)
  (unless (XIDEN_TRUST_BAD_DIGEST)
    (or (check-integrity (input-info-integrity input) tmp)
        (rex exn:fail:xiden:source:digest-mismatch input source)))
    (unless (XIDEN_TRUST_UNSIGNED)
      (or (check-signature (integrity-info-digest (input-info-integrity input))
                           (signature-info-body (input-info-signature input))
                           (signature-info-pubkey (input-info-signature input)))
          (rex exn:fail:xiden:source:signature-mismatch input source))))


; User may customize the means by which bytes are analyzed and fetched.
(define (get-get-source-info)
  (load-plugin 'get-source-info
               (λ () get-source-info)
               (λ (e) get-source-info)))


(define (get-input-size source)
  (or ((get-get-source-info) 'size source)
      (rex exn:fail:xiden:source:cannot-find-size source)))


(define (open-input-source source)
  (or ((get-get-source-info) 'port source)
      (rex exn:fail:xiden:source:cannot-open-port source)))


(define (call-with-input-source source proc)
  (define in (open-input-source source))
  (dynamic-wind void
                (λ () (proc in))
                (λ () (close-input-port in))))


(define (get-source-info/filesystem kind source)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (case kind
      [(size) (file-size source)]
      [(port) (open-input-file source)])))


(define (get-source-info/http kind source)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (case kind
      [(size)
       (define in (head-impure-port (string->url source)))
       (define headers (extract-all-fields (port->bytes in)))
       (string->number (bytes->string/utf-8 (extract-field #"content-length" headers)))]
      [(port) (get-pure-port #:redirections (XIDEN_DOWNLOAD_MAX_REDIRECTS)
                             (string->url source))])))


(define (get-source-info/package-output kind source)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (void))) ; TODO: Query package. Maybe build it first.


(define get-source-info
  (disjoin get-source-info/filesystem
           get-source-info/http))


(define (make-timeout-evt timeout handle)
  (handle-evt (alarm-evt (+ (current-inexact-milliseconds)
                            (XIDEN_FETCH_TIMEOUT_MS)))
              handle))


(define (copy-source-port in input-name source max-size tmp on-progress)
  (define timeout (XIDEN_FETCH_TIMEOUT_MS))
  (define buffer-size (inexact->exact (ceiling (* (XIDEN_FETCH_BUFFER_SIZE_MB) 1024 1024))))
  (define buffer (make-bytes buffer-size 0))

  (define (fail ctor)
    (rex ctor input-name source))

  (define (transfer out bytes-read)
    (on-progress source (/ bytes-read max-size))
    (if (> bytes-read max-size)
      (fail exn:fail:xiden:source:unexpected-size)
      (sync (make-timeout-evt timeout (λ () (fail exn:fail:xiden:source:fetch-timeout)))
            (handle-evt (read-bytes-avail!-evt buffer in)
                        (λ (variant)
                          (cond [(eof-object? variant)
                                 (void)]
                                [(and (number? variant) (> variant 0))
                                 (write-bytes buffer out 0 variant)
                                 (transfer out (+ bytes-read variant))]))))))

  (call-with-output-file tmp #:exists 'truncate/replace
    (λ (to-file) (transfer to-file 0))))


(module+ test
  (require racket/runtime-path
           rackunit
           mzlib/etc
           (submod "file.rkt" test)
           "setting.rkt")

  (define me (build-path (this-expression-source-directory) (this-expression-file-name)))
  (test-workspace
   "Fulfill an input from the file system"
   (define input-myself
     (input-info "hooray"
                 (list me)
                 (integrity-info 'sha384 (make-digest me 'sha384))
                 #f))

   (define progress-calls null)
   (parameterize ([(setting-derived-parameter XIDEN_TRUST_UNSIGNED) #t]
                  [(setting-derived-parameter XIDEN_FETCH_BUFFER_SIZE_MB) 0.001])
     (define-values (path errors)
       (fulfill-input input-myself
                      (λ A (set! progress-calls (cons A progress-calls)))))
     (check-pred null? errors)
     (define my-digest (make-digest me 'sha384))
     (define other-digest (make-digest path 'sha384))
     (check-equal? my-digest other-digest)
     (check-equal? (get-object-path my-digest)
                   (get-object-path other-digest)))

   (test-case "Record estimated progress on source download"
     (void (for/fold ([scalar 1])
                     ([progress (in-list progress-calls)])
             (check-equal? (car progress) me)
             (check-true (<= (cadr progress) scalar))
             (check-pred (real-in 0 1) (cadr progress))
             (cadr progress))))))
