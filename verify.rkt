#lang racket/base

; Define procedures used to verify custom data,
; so that other processes can continue.

(provide make-digest
         make-signature
         digest=?
         verify-signature
         well-formed-zcpkg-info?)

(require idiocket/contract
         idiocket/file
         idiocket/function
         idiocket/path
         idiocket/port
         idiocket/system
         "string.rkt"
         "zcpkg-info.rkt"
         "revision.rkt"
         "dependency.rkt")

(define openssl (find-executable-path "openssl"))

(define (run-openssl-command stdin-source . args)
  (define-values (sp from-stdout to-stdin from-stderr)
    (apply subprocess #f #f #f openssl args))

  (copy-port stdin-source to-stdin)
  (close-output-port to-stdin)

  (sync sp)
  (define exit-code (subprocess-status sp))

  (copy-port from-stderr (current-output-port))
  (define output (port->bytes from-stdout))

  (close-input-port from-stderr)
  (close-input-port from-stdout)
  (values exit-code output))

(define (make-digest target-path)
  (call-with-input-file target-path
    (λ (in)
      (run-openssl-command in
                           "dgst" "-binary" "-sha384"))))

(define (digest=? digest target-path)
  (define-values (exit-code other-digest) (make-digest target-path))
  (and (eq? exit-code 0)
       (bytes=? digest other-digest)))

(define (make-signature digest private-key-path)
  (run-openssl-command (open-input-bytes digest)
                       "pkeyutl"
                       "-sign"
                       "-inkey" private-key-path))

(define (verify-signature digest signature public-key)
  (define tmpsig (make-temporary-file))
  (call-with-output-file tmpsig
    #:exists 'truncate/replace
    (λ (o) (copy-port (open-input-bytes signature) o)))
  (define-values (exit-code msg)
    (run-openssl-command
     (open-input-bytes digest)
     "pkeyutl"
     "-verify"
     "-sigfile" tmpsig
     "-pubin" "-inkey" public-key))
  (eq? exit-code 0))


; TODO: Write hash algorithm-dependent check
(define (non-empty-bytes? b)
  (and (bytes? b)
       (> (bytes-length b) 0)))

(define well-formed-zcpkg-info?
  (curry passes-invariant-assertion?
         (struct/c zcpkg-info
                   name-string?
                   name-string?
                   name-string?
                   revision-number?
                   (listof name-string?)
                   (or/c #f path-string?)
                   (listof dependency-string?)
                   (or/c #f non-empty-bytes?)
                   (or/c #f non-empty-bytes?)
                   (or/c #f exact-positive-integer?))))
