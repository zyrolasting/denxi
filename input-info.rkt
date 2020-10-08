#lang racket/base

(require "cli-flag.rkt"
         "contract.rkt"
         "format.rkt"
         "integrity.rkt"
         "l10n.rkt"
         "localstate.rkt"
         "logged.rkt"
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
           flat-contract?]
          [input
           (->* (non-empty-string?
                 (non-empty-listof path-string?))
                ((or/c #f integrity-info?)
                 (or/c #f signature-info?))
                input-info?)]
          [call-with-input
           (-> input-info?
               (-> path-string? any)
               any)]
          [keep-input!
           (-> input-info?
               path-string?)]
          [input-ref
           (-> (listof input-info?)
               path-string?
               input-info?)]))


(struct input-info
  (name       ; The name to bind to bytes
   sources    ; Defines where said bytes come from
   integrity  ; Integrity information: Did I get the right bytes?
   signature) ; Signature for authentication: Did the bytes come from someone I trust?
  #:prefab)


(define well-formed-input-info/c
  (struct/c input-info
            file-name-string?
            (non-empty-listof string?)
            (or/c #f well-formed-integrity-info/c)
            (or/c #f well-formed-signature-info/c)))


(define (input name sources [integrity #f] [signature #f])
  (input-info name sources integrity signature))


(define (input-ref inputs str)
  (or (findf (λ (info) (equal? str (input-info-name info))) inputs)
      (raise-user-error 'input-ref
                        "~s does not match a declared input"
                        str)))


(define (call-with-input input proc)
  (define path (keep-input! input))
  (dynamic-wind void
                (λ () (proc path))
                (λ () (delete-file path))))


(define (keep-input! input)
  (define path (run+print-log (resolve-input input)))
  (if (eq? path FAILURE)
      (raise-user-error 'input-ref
                        "Could not resolve input ~a~nSources:~n~a~n"
                        (input-info-name input)
                        (join-lines (map ~a (input-info-sources input))))
      path))


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
  (define status
    (check-integrity #:trust-bad-digest (XIDEN_TRUST_BAD_DIGEST)
                     (input-info-integrity input)
                     (build-workspace-path (path-record-path file-record))))

  (define updated-messages
    (cons status messages))

  (if (passed-integrity-check? status)
      (check-input-signature input file-record source updated-messages)
      (values FAILURE updated-messages)))


(define (check-input-signature input file-record source messages)
  (define $message-ctor (get-signature-status input))
  (values (if (member $message-ctor
                      (list $signature-verified
                            $signature-unchecked
                            $signature-trust-unsigned))
              file-record
              FAILURE)
          (cons ($message-ctor (input-info-name input) source)
                messages)))

; This procedure uses most of the bindings from xiden/signature
; in terms of the runtime configuration. Look at the tests for
; xiden/signature to better understand what is happening.
;
; Due to use of CPS, each siginfo is eq? to (input-info-signature input),
; and you should read this procedure from bottom to top.
(define (get-signature-status input)
  (define (verify-signature-info public-key-path siginfo)
    (consider-signature-info public-key-path
                             (input-info-integrity input)
                             siginfo))

  (define (verify-public-key siginfo)
    (define trust-public-key?
      (if (XIDEN_TRUST_ANY_PUBLIC_KEY)
          (λ (p) #t)
          (bind-trusted-public-keys (XIDEN_TRUSTED_PUBLIC_KEYS))))
    (consider-public-key-trust #:trust-public-key? trust-public-key?
                               siginfo
                               verify-signature-info))

  (define (verify-unsigned siginfo)
    (consider-unsigned #:trust-unsigned (XIDEN_TRUST_UNSIGNED)
                       siginfo
                       verify-public-key))

  (consider-trust #:trust-bad-digest (XIDEN_TRUST_BAD_DIGEST)
                  (input-info-signature input)
                  verify-unsigned))
