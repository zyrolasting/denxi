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
          [make-input-expression-from-files
           (->* (path-string?
                 (-> bytes? path-string? (non-empty-listof any/c))
                 string?
                 path-string?)
                (#:local-name string?
                 #:md-algorithm md-algorithm/c
                 #:byte-encoding (or/c #f xiden-encoding/c)
                 (or/c #f path-string?))
                list?)]
          [input
           (->* (non-empty-string?)
                ((listof path-string?)
                 (or/c #f integrity-info?)
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

(define (input name [sources null] [integrity #f] [signature #f])
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


;-------------------------------------------------------------------------------
; Authoring aids

(define (make-input-expression-from-files
         path
         #:local-name [local-name (path->string (file-name-from-path path))]
         #:byte-encoding [byte-encoding 'base64]
         #:md-algorithm [message-digest-algorithm 'sha384]
         make-sources
         public-key-source
         private-key-path
         [private-key-password-path #f])
  (let ([digest (make-digest (expand-user-path path) message-digest-algorithm)]
        [make-byte-expression
         (if byte-encoding
             (λ (bstr) `(,byte-encoding ,(coerce-string (encode byte-encoding bstr))))
             values)])
    `(input ,local-name
            (sources . ,(make-sources digest path))
            (integrity ',message-digest-algorithm ,(make-byte-expression digest))
            (signature ,public-key-source
                       ,(make-byte-expression
                         (make-signature-bytes
                          digest
                          (expand-user-path private-key-path)
                          (and private-key-password-path
                               (expand-user-path private-key-password-path))))))))

(module+ test
  (require rackunit)

  (define abstract-input-args
    '(("x" ())
      ("x" () #f)
      ("x" () #f #f)))

  (define concrete-input-args
    '(("x" ("x") #f #f)))

  (for ([aargs (in-list abstract-input-args)])
    (let ([i (apply input aargs)])
      (test-case (format "~s is abstract" i)
        (check-true (abstract-input-info/c i))
        (check-false (concrete-input-info/c i)))))

  (for ([cargs (in-list concrete-input-args)])
    (let ([i (apply input cargs)])
      (test-case (format "~s is concrete" i)
        (check-false (abstract-input-info/c i))
        (check-true (concrete-input-info/c i))))))
