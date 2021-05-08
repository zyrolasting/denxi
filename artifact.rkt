#lang racket/base

(require racket/contract)
(provide
 (struct-out artifact-info)
 (contract-out
  [verify-artifact
   (-> artifact-info?
       path-record?
       (subprogram/c void?))]
  [fetch-artifact
   (-> string?
       artifact-info?
       (subprogram/c path-record?))]
  [artifact
   (->* (source-variant?)
        ((or/c #f well-formed-integrity-info/c)
         (or/c #f well-formed-signature-info/c))
        artifact-info?)]))


(require "format.rkt"
         "integrity.rkt"
         "localstate.rkt"
         "subprogram.rkt"
         "monad.rkt"
         "openssl.rkt"
         "port.rkt"
         "printer.rkt"
         "signature.rkt"
         "source.rkt")


(struct artifact-info
  (source      ; Defines where bytes come from
   integrity   ; Integrity information: Did I get the right bytes?
   signature)) ; Signature for authentication: Did the bytes come from someone I trust?


(define (artifact source [integrity #f] [signature #f])
  (artifact-info source integrity signature))


(define (verify-artifact arti record)
  (mdo (check-artifact-integrity arti (path-record-path record))
       (check-artifact-signature arti (path-record-path record))
       (subprogram-unit (void))))


(define (fetch-artifact name arti)
  (subprogram-fetch name
                    (artifact-info-source arti)
                    (λ (in est-size)
                      (make-addressable-file
                       #:cache-key (make-source-key (artifact-info-source arti))
                       #:max-size (mebibytes->bytes (XIDEN_FETCH_TOTAL_SIZE_MB))
                       #:buffer-size (mebibytes->bytes (XIDEN_FETCH_BUFFER_SIZE_MB))
                       #:timeout-ms (XIDEN_FETCH_TIMEOUT_MS)
                       #:on-status (make-on-status (current-message-formatter))
                       name
                       in est-size))))


(define ((make-on-status formatter) m)
  (if ($transfer:progress? ($transfer:scope-message m))
      (printf "\r~a~a" (formatter m)
              (if (equal? ($transfer:progress-bytes-read ($transfer:scope-message m))
                          ($transfer:progress-max-size ($transfer:scope-message m)))
                  "\n" ""))
      (write-message #:newline? #f m formatter)))


(define-subprogram (check-artifact-integrity arti workspace-relative-path)
  (define status
    (check-integrity #:trust-bad-digest (XIDEN_TRUST_BAD_DIGEST)
                     #:trust-message-digest-algorithms (XIDEN_TRUST_CHFS)
                     (artifact-info-integrity arti)
                     (if (artifact-info-integrity arti)
                         (make-digest (build-workspace-path workspace-relative-path)
                                      (integrity-info-algorithm (artifact-info-integrity arti)))
                         #"")))
                     
  ($attach (or ($integrity-ok? status) FAILURE)
           status))


(define-subprogram (check-artifact-signature arti path)
  (define siginfo (artifact-info-signature arti))
  (define intinfo (artifact-info-integrity arti))

  ; The module level contract insists on a path string due
  ; to the file-oriented implementation in signature.rkt, but
  ; the signature info may not be declared. Use an unlikely
  ; path as a workaround, since other checks won't actually
  ; use the path.
  (define public-key-path
    (if (signature-info? siginfo)
        (fetch-signature-payload (signature-info-pubkey siginfo)
                                 (λ _ "__nowhere"))
        "__nowhere"))

  (define trust-public-key?
    (if (XIDEN_TRUST_ANY_PUBLIC_KEY)
        (λ (p) #t)
        (bind-trust-list (XIDEN_TRUST_PUBLIC_KEYS))))

  (define status
    (check-signature #:public-key-path public-key-path
                     #:trust-unsigned (XIDEN_TRUST_UNSIGNED)
                     #:trust-bad-digest (XIDEN_TRUST_BAD_DIGEST)
                     #:trust-public-key? trust-public-key?
                     siginfo
                     intinfo))

  ($attach (or ($signature-ok? status)
               FAILURE)
           status))
