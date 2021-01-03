#lang racket/base

; Define a package input as a request for exact bytes from at least
; one source. Requires knowledge of source.rkt, integrity.rkt and
; signature.rkt.

(require "cli-flag.rkt"
         "codec.rkt"
         "contract.rkt"
         "format.rkt"
         "integrity.rkt"
         "l10n.rkt"
         "localstate.rkt"
         "logged.rkt"
         "message.rkt"
         "monad.rkt"
         "path.rkt"
         "port.rkt"
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
           (-> concrete-input-info/c logged?)]
          [well-formed-input-info/c
           flat-contract?]
          [abstract-input-info/c
           flat-contract?]
          [concrete-input-info/c
           flat-contract?]
          [make-input-info
           (->* (non-empty-string?)
                ((listof path-string?)
                 (or/c #f integrity-info?)
                 (or/c #f signature-info?))
                input-info?)]
          [release-input
           (-> input-info? (logged/c void?))]
          [find-input
           (-> (listof input-info?)
               path-string?
               (logged/c input-info?))]
          [input-ref
           (-> string? (logged/c input-info?))]
          [current-inputs
           (parameter/c (listof input-info?))]))


(define+provide-message $input-not-found (name))

(struct input-info
  (name       ; The name to bind to bytes
   sources    ; Defines where said bytes come from
   integrity  ; Integrity information: Did I get the right bytes?
   signature) ; Signature for authentication: Did the bytes come from someone I trust?
  #:prefab)


(define well-formed-input-info/c
  (struct/c input-info
            file-name-string?
            (listof path-string?)
            (or/c #f well-formed-integrity-info/c)
            (or/c #f well-formed-signature-info/c)))


(define abstract-input-info/c
  (struct/c input-info
            file-name-string?
            null?
            #f
            #f))

(define concrete-input-info/c
  (and/c well-formed-input-info/c
         (not/c abstract-input-info/c)))

(define current-inputs (make-parameter null))

(define (input-ref name)
  (find-input (current-inputs) name))

(define (make-input-info name [sources null] [integrity #f] [signature #f])
  (input-info name sources integrity signature))

(define-logged (find-input inputs name)
  (define result (findf (λ (info) (equal? name (input-info-name info))) inputs))
  (if result
      ($use result)
      ($fail ($input-not-found name))))

(define-logged (release-input input)
  ($use (delete-file (input-info-name input))))

(define (resolve-input info)
  (mdo pathrec-or-#f  := (logged-unit (find-existing-path-record info))
       link-name      := (logged-unit (input-info-name info))
       file-record    := (fetch-exact-input info pathrec-or-#f)
       link-record    := (logged-unit (make-addressable-link file-record link-name))
       (logged-unit link-name)))

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
         (define-values (fetch-result messages*)
           (run-log (fetch-input info) messages))
         (if (path-record? (fetch-state-result fetch-result))
             (check-input-integrity info
                                    (fetch-state-result fetch-result)
                                    (fetch-state-source fetch-result)
                                    messages*)
             (values FAILURE messages*))))))

(define (fetch-input info)
  (fetch (input-info-name info)
         (input-info-sources info)
         (λ (in est-size)
           (make-addressable-file
            #:max-size (mebibytes->bytes (XIDEN_FETCH_TOTAL_SIZE_MB))
            #:buffer-size (mebibytes->bytes (XIDEN_FETCH_BUFFER_SIZE_MB))
            #:timeout-ms (XIDEN_FETCH_TIMEOUT_MS)
            #:on-status (let ([formatter (get-message-formatter)])
                          (λ (m)
                            (if ($transfer:progress? ($transfer:scope-message m))
                                (printf "\r~a~a" (formatter m)
                                        (if (equal? ($transfer:progress-bytes-read ($transfer:scope-message m))
                                                    ($transfer:progress-max-size ($transfer:scope-message m)))
                                            "\n" ""))
                                (write-message #:newline? #f m formatter))))
            (input-info-name info)
            in est-size))))

(define (find-existing-path-record info)
  (and (input-info-integrity info)
       (integrity-info-digest (input-info-integrity info))
       (find-path-record (integrity-info-digest (input-info-integrity info)))))

(define ($regarding-input+source input source m)
  ($regarding ($show-string (input-info-name input))
              ($regarding ($show-string source)
                          m)))

(define (check-input-integrity input file-record source messages)
  (define status
    (check-integrity #:trust-bad-digest (XIDEN_TRUST_BAD_DIGEST)
                     (input-info-integrity input)
                     (build-workspace-path (path-record-path file-record))))

  (define updated-messages
    (cons ($regarding-input+source input source status)
          messages))

  (if ($integrity-ok? status)
      (check-input-signature input file-record source updated-messages)
      (values FAILURE updated-messages)))


(define (check-input-signature input file-record source messages)
  (define siginfo (input-info-signature input))
  (define intinfo (input-info-integrity input))

  ; The module level contract insists on a path string due
  ; to the file-oriented implementation in signature.rkt, but
  ; the signature info may not be declared. Use an unlikely
  ; path as a workaround, since other checks won't actually
  ; use the path.
  (define public-key-path
    (if (signature-info? siginfo)
        (get-public-key-path (signature-info-pubkey siginfo))
        "/nowhere"))

  (define trust-public-key?
    (if (XIDEN_TRUST_ANY_PUBLIC_KEY)
        (λ (p) #t)
        (bind-trust-list (XIDEN_TRUSTED_PUBLIC_KEYS))))

  (define status
    (check-signature #:public-key-path public-key-path
                     #:trust-unsigned (XIDEN_TRUST_UNSIGNED)
                     #:trust-bad-digest (XIDEN_TRUST_BAD_DIGEST)
                     #:trust-public-key? trust-public-key?
                     siginfo
                     intinfo))

  (values (if ($signature-ok? status)
              file-record
              FAILURE)
          (cons ($regarding-input+source input source status)
                messages)))

(module+ test
  (require rackunit)

  (define abstract-input-args
    '(("x" ())
      ("x" () #f)
      ("x" () #f #f)))

  (define concrete-input-args
    '(("x" ("x") #f #f)))

  (for ([aargs (in-list abstract-input-args)])
    (let ([i (apply make-input-info aargs)])
      (test-case (format "~s is abstract" i)
        (check-true (abstract-input-info/c i))
        (check-false (concrete-input-info/c i)))))

  (for ([cargs (in-list concrete-input-args)])
    (let ([i (apply make-input-info cargs)])
      (test-case (format "~s is concrete" i)
        (check-false (abstract-input-info/c i))
        (check-true (concrete-input-info/c i))))))
