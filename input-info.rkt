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


(define (fetch-source input source)
  (define input-name (input-info-name input))
  (define expected-size (get-input-size source))
  (define in (open-input-source source))
  (define tmp (make-temporary-file))
  (with-handlers ([values (λ (e) (delete-file* tmp) (raise e))])
    (copy-port/report-progress in input-name source expected-size tmp)
    (raise-unless-postconditions-met input-name tmp)
    (define path (get-xiden-object-path (make-digest tmp)))
    (rename-file-or-directory tmp path)
    path))


(define (fulfill-input input)
  (for/fold ([path #f]
             [errors null])
            ([source (in-list (input-info-sources input))])
    (with-handlers ([exn:fail:xiden:source? (λ (e) (values #f (cons e errors)))])
      (values (or path (fetch-source input source))
              (reverse errors)))))


(define (raise-unless-postconditions-met input-name source src tmp)
  (for ([pre (in-list (get-fetch-postconditions))])
    (pre input-name source src tmp)))


(define (get-fetch-postconditions)
  (if (XIDEN_TRUST_BAD_DIGEST)
      null
      (if (XIDEN_TRUST_BAD_SIGNATURE)
          (list raise-unless-integrous)
          (list raise-unless-integrous
                raise-unless-authenticated))))


(define (raise-unless-authenticated input-name source src tmp)
  (or (check-signature (integrity-info-digest (input-info-integrity src))
                       (signature-info-body (input-info-signature src))
                       (signature-info-pubkey (input-info-signature src)))
      (rex exn:fail:xiden:source:signature-mismatch input-name src source)))


(define (raise-unless-integrous input-name source src tmp)
  (or (check-integrity (input-info-integrity src) tmp)
      (rex exn:fail:xiden:source:digest-mismatch input-name src source)))


; User may customize the means by which bytes are analyzed and fetched.
(define (get-get-source-info)
  (define (fallback . _) get-source-info)
  (dynamic-require/mod 'get-source-info fallback fallback))


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


(define get-source-info
  (disjoin get-source-info/filesystem
           get-source-info/http))


(define (get-xiden-object-path digest)
  (define full
    (bytes->string/utf-8
     (encode 'base32 digest)))
  (build-workspace-path
   "var/xiden/objects"
   (substring full 0
              (min (string-length full) 64))))

(define (copy-port/report-progress in input-name source max-size tmp on-progress)
  (define buffer-size/mb (XIDEN_FETCH_BUFFER_SIZE_MB))
  (define buffer-size (quotient buffer-size/mb (* 1024 1024)))

  (call-with-output-file tmp #:exists 'truncate/replace
    (λ (to-file)
      (define th
        (thread
         (λ ()
           (let loop ([bytes-read 0])
             (when (> bytes-read max-size)
               (raise ((exc exn:fail:xiden:source:unexpected-size input-name source))))
             (on-progress (/ bytes-read max-size))

             (sync (handle-evt
                    (alarm-evt (+ (current-inexact-milliseconds)
                                  (XIDEN_FETCH_TIMEOUT_MS)))
                    (λ (alarm)
                      (rex exn:fail:xiden:source:fetch-timeout)))

                   (handle-evt
                    (read-bytes-evt buffer-size in)
                    (λ (buffer)
                      (write-bytes buffer to-file)
                      (loop (min max-size (+ bytes-read buffer-size))))))))))

      (sync/enable-break th))))
