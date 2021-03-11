#lang racket/base

; Define a package input as a request for exact bytes from at least
; one source. Requires knowledge of source.rkt, integrity.rkt and
; signature.rkt.

(require "format.rkt"
         "integrity.rkt"
         "localstate.rkt"
         "logged.rkt"
         "message.rkt"
         "monad.rkt"
         "port.rkt"
         "printer.rkt"
         "signature.rkt"
         "source.rkt"
         "strict-rc.rkt"
         "string.rkt"
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
                (source?
                 (or/c #f integrity-info?)
                 (or/c #f signature-info?))
                input-info?)]
          [release-input
           (-> input-info? (logged/c void?))]
          [keep-input
           (-> string? logged?)]
          [find-input
           (-> (listof input-info?)
               path-string?
               (logged/c input-info?))]
          [input-ref
           (-> string? (logged/c input-info?))]
          [current-inputs
           (parameter/c (listof input-info?))]))


(define+provide-message $input (name))
(define+provide-message $input:not-found $input ())

(struct input-info
  (name       ; The name to bind to bytes
   source     ; Defines where said bytes come from
   integrity  ; Integrity information: Did I get the right bytes?
   signature) ; Signature for authentication: Did the bytes come from someone I trust?
  #:prefab)

(define well-formed-input-info/c
  (struct/c input-info
            file-name-string?
            (or/c #f source?)
            (or/c #f well-formed-integrity-info/c)
            (or/c #f well-formed-signature-info/c)))


(define abstract-input-info/c
  (struct/c input-info
            file-name-string?
            #f
            #f
            #f))

(define concrete-input-info/c
  (and/c well-formed-input-info/c
         (not/c abstract-input-info/c)))

(define current-inputs (make-parameter null))

(define (input-ref name)
  (find-input (current-inputs) name))

(define (make-input-info name [source #f] [integrity #f] [signature #f])
  (input-info name source integrity signature))

(define-logged (find-input inputs name)
  (define result (findf (λ (info) (equal? name (input-info-name info))) inputs))
  (if result
      ($use result)
      ($fail ($input:not-found name))))

(define (keep-input name)
  (mdo i := (input-ref name)
       (resolve-input i)))

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
         (check-input-integrity info pathrec-or-#f messages)))
      (mdo pathrec := (fetch-input info)
           (logged
            (λ (messages)
              (check-input-integrity info pathrec messages))))))

(define (fetch-input info)
  (logged-fetch (input-info-name info)
                (input-info-source info)
                (λ (in est-size)
                  (make-addressable-file
                   #:cache-key (make-source-key (input-info-source info))
                   #:max-size (mebibytes->bytes (rc-ref 'XIDEN_FETCH_TOTAL_SIZE_MB))
                   #:buffer-size (mebibytes->bytes (rc-ref 'XIDEN_FETCH_BUFFER_SIZE_MB))
                   #:timeout-ms (rc-ref 'XIDEN_FETCH_TIMEOUT_MS)
                   #:on-status (make-on-status (current-message-formatter))
                   (input-info-name info)
                   in est-size))))


(define (make-on-status formatter)
  (λ (m)
    (if ($transfer:progress? ($transfer:scope-message m))
        (printf "\r~a~a" (formatter m)
                (if (equal? ($transfer:progress-bytes-read ($transfer:scope-message m))
                            ($transfer:progress-max-size ($transfer:scope-message m)))
                    "\n" ""))
        (write-message #:newline? #f m formatter))))


(define (find-existing-path-record info)
  (and (input-info-integrity info)
       (integrity-info-digest (input-info-integrity info))
       (find-path-record (integrity-info-digest (input-info-integrity info)))))

(define ($regarding-input input m)
  ($regarding ($show-string (input-info-name input)) m))

(define (check-input-integrity input file-record messages)
  (define status
    (check-integrity #:trust-bad-digest (rc-ref 'XIDEN_TRUST_BAD_DIGEST)
                     #:trust-message-digest-algorithms (rc-ref 'XIDEN_TRUST_MESSAGE_DIGEST_ALGORITHMS)
                     (input-info-integrity input)
                     (build-workspace-path (path-record-path file-record))))

  (define updated-messages
    (cons ($regarding-input input status)
          messages))

  (if ($integrity-ok? status)
      (check-input-signature input file-record updated-messages)
      (values FAILURE updated-messages)))


(define (check-input-signature input file-record messages)
  (define siginfo (input-info-signature input))
  (define intinfo (input-info-integrity input))

  ; The module level contract insists on a path string due
  ; to the file-oriented implementation in signature.rkt, but
  ; the signature info may not be declared. Use an unlikely
  ; path as a workaround, since other checks won't actually
  ; use the path.
  (define public-key-path
    (if (signature-info? siginfo)
        (fetch-signature-payload (signature-info-pubkey siginfo)
                                 (λ _ "/nowhere"))
        "/nowhere"))

  (define trust-public-key?
    (if (rc-ref 'XIDEN_TRUST_ANY_PUBLIC_KEY)
        (λ (p) #t)
        (bind-trust-list (rc-ref 'XIDEN_TRUST_PUBLIC_KEYS))))

  (define status
    (check-signature #:public-key-path public-key-path
                     #:trust-unsigned (rc-ref 'XIDEN_TRUST_UNSIGNED)
                     #:trust-bad-digest (rc-ref 'XIDEN_TRUST_BAD_DIGEST)
                     #:trust-public-key? trust-public-key?
                     siginfo
                     intinfo))

  (values (if ($signature-ok? status)
              file-record
              FAILURE)
          (cons ($regarding-input input status)
                messages)))

(module+ test
  (require rackunit)

  (define abstract-input-args
    '(("x" #f)
      ("x" #f #f)
      ("x" #f #f #f)))

  (define concrete-input-args
    (list (list "x" (coerce-source "x") #f #f)))

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
