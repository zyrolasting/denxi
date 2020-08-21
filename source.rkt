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
         "exn.rkt"
         "integrity.rkt"
         "mod.rkt"
         "rc.rkt"
         "signature.rkt")

(struct source
  (expressions      ; Human expressions used to fetch bits
   integrity        ; Integrity information
   signature        ; Signature for authentication
   preconditions    ; Assertions that an instance of this structure must pass before fetching bits
   postconditions)) ; Assertions that bits fetched using an instance of this structure must pass

(define (raise-unless-preconditions-met input-name src)
  (for ([pre (in-list (get-fetch-preconditions))])
    (pre input-name src)))

(define (raise-unless-postconditions-met input-name exp-id src tmp)
  (for ([pre (in-list (get-fetch-preconditions))])
    (pre input-name exp-id src tmp)))

(define (get-fetch-preconditions)
  (if (XIDEN_TRUST_BAD_DIGEST)
      null ; If you are willing to trust arbitrary content, then there's nothing to check.
      (if (XIDEN_TRUST_UNSIGNED)
          (list raise-unless-integrity-info-declared)
          (list raise-unless-integrity-info-declared
                raise-unless-signature-info-declared))))

(define (get-fetch-postconditions)
  (if (XIDEN_TRUST_BAD_DIGEST)
      null
      (if (XIDEN_TRUST_BAD_SIGNATURE)
          (list raise-unless-integrous)
          (list raise-unless-integrous
                raise-unless-authenticated))))

(define (raise-unless-signature-info-declared input-name src)
  (or (well-formed-signature-info? (source-signature src))
      (raise (exc exn:fail:xiden:source:signature:missing src))))

(define (raise-unless-authenticated exp-id src tmp)
  (or (verify-signature (integrity-info-digest (source-integrity int))
                        (signature-info-body (source-signature src))
                        (signature-info-pubkey sig))
      (raise ((exc exn:fail:xiden:source:signature:mismatch src exp-id)))))

(define (raise-unless-integrous exp-id src tmp)
  (or (check-integrity (source-integrity src) tmp)
      (raise ((exc exn:fail:xiden:source:digest:mismatch src exp-id)))))


; User may customize the means by which bytes are analyzed and fetched.
(define (get-get-source-info)
  (dynamic-require/mod 'get-source-info
                       (const get-source-info)))


(define (get-input-size key)
  (or ((get-get-source-info) 'size key)
      (raise ((exc exn:fail:xiden:source:cannot-find-size key)
              "Cannot determine size of source ~a" key))))

(define (open-input-source key)
  (raise-unless-preconditions-met)
  (or ((get-get-source-info) 'port key)
      (raise ((exc exn:fail:xiden:source:cannot-open-port key)
              "Cannot open source ~a" key))))


(define (call-with-input-source src proc)
  (define in (open-input-source src))
  (dynamic-wind void
                (λ () (proc in))
                (λ () (close-input-port in))))


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


(define (get-xiden-object-path digest)
  (define full
    (bytes->string/utf-8
     (encode 'base32 digest)))
  (build-workspace-path
   "var/xiden/objects"
   (substring full 0
              (min (string-length full) 64))))


(define (make-xiden-object-file src)
  ; Copy the source to the filesystem immediately.
  ; We don't know how big it is, so we don't want
  ; to buffer the entire port's contents in memory.
  (define in (open-input-source (source-origin src)))
  (define tmp (make-temporary-file))
  (with-handlers ([values (λ (e) (delete-file* tmp) (raise e))])
    (call-with-output-file tmp #:exists 'truncate/replace
      (λ (to-file) (copy-port in to-file)))
    (raise-unless-postconditions-met)
    (rename-file-or-directory tmp (get-xiden-object-path (make-digest tmp)))))
