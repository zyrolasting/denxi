#lang racket/base

(require "contract.rkt"
         "integrity.rkt"
         "localstate.rkt"
         "message.rkt"
         "printer.rkt"
         "rc.rkt"
         "signature.rkt"
         "string.rkt"
         "source.rkt")

(provide (struct-out input-info)
         (contract-out
          [get-fetch-sources
           (-> input-info? (listof string?))]
          [check-input-integrity
           (-> input-info? fetch-state? $input-integrity-status?)]
          [check-input-signature
           (-> input-info? fetch-state? $input-signature-status?)]
          [well-formed-input-info/c
           flat-contract?]))


(define+provide-message $input (name))
(define+provide-message $input-integrity-status    (source))
(define+provide-message $input-integrity-verified  $input-integrity-status ())
(define+provide-message $input-integrity-violation $input-integrity-status ())
(define+provide-message $input-integrity-assumed   $input-integrity-status ())
(define+provide-message $input-integrity-missing   $input-integrity-status ())

(define+provide-message $input-signature-status         (source))
(define+provide-message $input-signature-missing        $input-signature-status ())
(define+provide-message $input-signature-unchecked      $input-signature-status ())
(define+provide-message $input-signature-trust-unsigned $input-signature-status ())
(define+provide-message $input-signature-verified       $input-signature-status ())
(define+provide-message $input-signature-mismatch       $input-signature-status ())


(struct input-info
  (name       ; The name to bind to bytes
   sources    ; Defines where said bytes come from
   integrity  ; Integrity information: Did I get the right bytes?
   signature) ; Signature for authentication: Did the bytes come from someone I trust?
  #:prefab)


(define well-formed-input-info/c
  (struct/c input-info
            name-string?
            (non-empty-listof string?)
            (or/c #f well-formed-integrity-info/c)
            (or/c #f well-formed-signature-info/c)))


; I add any cached path as a source so that it goes through
; validation. This captures local tampering.
(define (get-fetch-sources info)
  (with-handlers ([exn? (λ _ (input-info-sources info))])
    (define path
      (build-object-path
       (integrity-info-digest
        (input-info-integrity info))))
    (if (file-exists? path)
        (cons (path->string path)
              (input-info-sources info))
        (input-info-sources info))))


(define (check-input-integrity input fetch-st)
  (define $message-ctor
    (if (XIDEN_TRUST_BAD_DIGEST)
        $input-integrity-assumed
        (if (input-info-integrity input)
            (if (check-integrity (input-info-integrity input) (fetch-state-path fetch-st))
                $input-integrity-verified
                $input-integrity-violation)
            $input-integrity-missing)))

  ($message-ctor (input-info-name input)
                 (fetch-state-source fetch-st)))

(define (check-input-signature input fetch-st)
  (define $message-ctor
    (if (XIDEN_TRUST_BAD_DIGEST)
        $input-signature-unchecked
        (if (XIDEN_TRUST_UNSIGNED)
            $input-signature-trust-unsigned
            (if (input-info-signature input)
                (if (check-signature (integrity-info-digest (input-info-integrity input))
                                     (signature-info-body (input-info-signature input))
                                     (signature-info-pubkey (input-info-signature input)))
                    $input-signature-verified
                    $input-signature-mismatch)
                $input-signature-missing))))

  ($message-ctor (input-info-name input)
                 (fetch-state-source fetch-st)))


#;(define-message-formatter input-message-formatter
  [($input-integrity-violation input-name source)
   (format (~a "~a failed its integrity check.~n"
               "While unsafe, you can force installation using ~a.")
           input-name
           (setting-long-flag XIDEN_TRUST_BAD_DIGEST))]

  [($fetch-signature-mismatch input source)
   (format (~a "~s's signature does not match any trusted public key.~n"
               "While unsafe, you can trust bad signatures using ~a.")
           source
           (setting-long-flag XIDEN_TRUST_BAD_SIGNATURE))]

  [($input-signature-missing (fetch-info name _ _ _) source)
   (format (~a "~a does not have a signature. If you are prototyping your own package, this is expected.~n"
               "If you got the package from the Internet, then exercise caution!~n"
               "To trust unsigned packages, use ~a.")
           name
           (setting-long-flag XIDEN_TRUST_UNSIGNED))])
