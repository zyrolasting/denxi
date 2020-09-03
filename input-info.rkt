#lang racket/base

(require "contract.rkt"
         "format.rkt"
         "integrity.rkt"
         "localstate.rkt"
         "message.rkt"
         "monad.rkt"
         "printer.rkt"
         "rc.rkt"
         "setting.rkt"
         "signature.rkt"
         "string.rkt"
         "source.rkt")

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
            name-string?
            (non-empty-listof path-string?)
            (or/c #f well-formed-integrity-info/c)
            (or/c #f well-formed-signature-info/c)))


(define (resolve-input info)
  (do path-or-#f <- (logged-unit (get-anticipated-input-path info))
      result <- (fetch-exact-input info path-or-#f)
      (return result)))


(define (fetch-exact-input info path-or-#f)
  (if (and path-or-#f (file-exists? path-or-#f))
      (logged
       (λ (messages)
         (check-input-integrity info
                                path-or-#f
                                (path->string path-or-#f)
                                messages)))
      (logged
       (λ (messages)
         (define-values (result logged) (run-log (fetch-input info) messages))
         (if (fetch-state-path result)
             (check-input-integrity info
                                    (fetch-state-path result)
                                    (fetch-state-source result)
                                    logged)
             (values FAILURE logged))))))


(define (fetch-input info)
  (fetch (input-info-name info)
         (input-info-sources info)
         (λ (in est-size)
           (make-addressable-file
            (input-info-name info)
            in est-size))))


(define (get-anticipated-input-path info)
  (and (input-info-integrity info)
       (integrity-info-digest (input-info-integrity info))
       (build-object-path
        (integrity-info-digest
         (input-info-integrity info)))))


(define (check-input-integrity input path source messages)
  (define $message-ctor
    (if (XIDEN_TRUST_BAD_DIGEST)
        $input-integrity-assumed
        (if (input-info-integrity input)
            (if (check-integrity (input-info-integrity input) path)
                $input-integrity-verified
                $input-integrity-violation)
            $input-integrity-missing)))

  (define updated-messages
    (cons ($message-ctor (input-info-name input) source)
          messages))

  (if (member $message-ctor
              (list $input-integrity-verified
                    $input-integrity-assumed))
      (check-input-signature input path source updated-messages)
      (values FAILURE updated-messages)))


(define (check-input-signature input path source messages)
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

  (values (if (member $message-ctor
                      (list $input-signature-verified
                            $input-signature-unchecked
                            $input-signature-trust-unsigned))
              path
              FAILURE)
          (cons ($message-ctor (input-info-name input) source)
                messages)))


(define+provide-message-formatter format-input-message
  [($input-resolve-start name)
   (format "Resolving input ~s" name)]

  [($input-integrity-violation name source)
   (format (~a "~a failed its integrity check.~n"
               "While unsafe, you can force installation using ~a.")
           name
           (setting-long-flag XIDEN_TRUST_BAD_DIGEST))]

  [($input-signature-mismatch name source)
   (format (~a "~s's signature on ~s does not match any trusted public key.~n"
               "While unsafe, you can trust bad signatures using ~a.")
           name
           source
           (setting-long-flag XIDEN_TRUST_BAD_SIGNATURE))]

  [($input-signature-missing name source)
   (format (~a "~a does not have a signature. If you are prototyping your own package, this is expected.~n"
               "If you got the package from the Internet, then exercise caution!~n"
               "To trust unsigned packages, use ~a.")
           name
           (setting-long-flag XIDEN_TRUST_UNSIGNED))]

  [($input-integrity-verified name source)
   (format "Integrity verified for input ~s from ~s" name source)]

  [($input-integrity-assumed name source)
   (format "Dangerously trusting input ~s from ~s" name source)]

  [($input-signature-unchecked name source)
   (format "Not checking signature for input ~s from ~s"
           name source)]

  [($input-integrity-missing name source)
   (format (~a "~a does not declare integrity information.~n"
               "If you are prototyping your own package, this is expected.~n"
               "Otherwise, please declare integrity information for safety.")
           name)]

  [($input-signature-trust-unsigned name source)
   (format "Trusting unsigned input ~s from ~s" name source)]

  [($input-signature-verified name source)
   (format "Signature verified for input ~s from ~s" name source)]

  [($input-signature-mismatch name source)
   (format "Signature mismatch for input ~s from ~s" name source)])
