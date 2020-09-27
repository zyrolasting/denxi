#lang racket/base

(require "cli-flag.rkt"
         "contract.rkt"
         "format.rkt"
         "integrity.rkt"
         "l10n.rkt"
         "localstate.rkt"
         "message.rkt"
         "monad.rkt"
         "printer.rkt"
         "rc.rkt"
         "setting.rkt"
         "signature.rkt"
         "string.rkt"
         "source.rkt"
         "workspace.rkt")

(provide (struct-out input-info)
         (contract-out
          [resolve-input
           (-> input-info? logged?)]
          [well-formed-input-info/c
           flat-contract?]))


(define+provide-message $input (name))
(define+provide-message $input-resolve-start       $input ())
(define+provide-message $input-integrity-status    $input (source))
(define+provide-message $input-integrity-verified  $input-integrity-status ())
(define+provide-message $input-integrity-violation $input-integrity-status ())
(define+provide-message $input-integrity-assumed   $input-integrity-status ())
(define+provide-message $input-integrity-missing   $input-integrity-status ())

(define+provide-message $input-signature-status         $input (source))
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
            file-name-string?
            (non-empty-listof path-string?)
            (or/c #f well-formed-integrity-info/c)
            (or/c #f well-formed-signature-info/c)))


(define (resolve-input info)
  (do pathrec-or-#f  <- (logged-unit (find-existing-path-record info))
      link-name      <- (logged-unit (input-info-name info))
      file-record    <- (fetch-exact-input info pathrec-or-#f)
      link-record    <- (logged-unit (make-addressable-link file-record link-name))
      (return link-name)))


(define (fetch-exact-input info pathrec-or-#f)
  (if (path-record? pathrec-or-#f)
      (logged
       (λ (messages)
         (check-input-integrity info
                                pathrec-or-#f
                                (path-record-path pathrec-or-#f)
                                messages)))
      (logged
       (λ (messages)
         (define-values (fetch-result logged)
           (run-log (fetch-input info) messages))
         (if (path-record? (fetch-state-result fetch-result))
             (check-input-integrity info
                                    (fetch-state-result fetch-result)
                                    (fetch-state-source fetch-result)
                                    logged)
             (values FAILURE logged))))))


(define (fetch-input info)
  (fetch (input-info-name info)
         (input-info-sources info)
         (λ (in est-size)
           (make-addressable-file
            #:on-status (let ([formatter (get-message-formatter)])
                          (λ (m) (write-message m formatter)))
            (input-info-name info)
            in est-size))))


(define (find-existing-path-record info)
  (and (input-info-integrity info)
       (integrity-info-digest (input-info-integrity info))
       (find-path-record (integrity-info-digest (input-info-integrity info)))))


(define (check-input-integrity input file-record source messages)
  (define $message-ctor
    (if (XIDEN_TRUST_BAD_DIGEST)
        $input-integrity-assumed
        (if (input-info-integrity input)
            (if (check-integrity (input-info-integrity input)
                                 (build-workspace-path (path-record-path file-record)))
                $input-integrity-verified
                $input-integrity-violation)
            $input-integrity-missing)))

  (define updated-messages
    (cons ($message-ctor (input-info-name input) source)
          messages))

  (if (member $message-ctor
              (list $input-integrity-verified
                    $input-integrity-assumed))
      (check-input-signature input file-record source updated-messages)
      (values FAILURE updated-messages)))


(define (check-input-signature input file-record source messages)
  (define $message-ctor
    (if (XIDEN_TRUST_BAD_DIGEST)
        $input-signature-unchecked
        (if (input-info-signature input)
            (if (check-signature (integrity-info-digest (input-info-integrity input))
                                 (signature-info-pubkey (input-info-signature input))
                                 (signature-info-body (input-info-signature input)))
                $input-signature-verified
                $input-signature-mismatch)
            (if (XIDEN_TRUST_UNSIGNED)
                $input-signature-trust-unsigned
                $input-signature-missing))))

  (values (if (member $message-ctor
                      (list $input-signature-verified
                            $input-signature-unchecked
                            $input-signature-trust-unsigned))
              file-record
              FAILURE)
          (cons ($message-ctor (input-info-name input) source)
                messages)))
