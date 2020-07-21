#lang racket/base

; Define procedures used to verify custom data,
; so that other processes can continue.

(provide make-digest
         make-signature
         digest=?
         verify-signature
         validate-zcpkg-info
         integrous-artifact?
         authenticated-provider?)

(require racket/file
         racket/function
         racket/path
         racket/port
         racket/system
         "contract.rkt"
         "string.rkt"
         "url.rkt"
         "zcpkg-info.rkt"
         "zcpkg-settings.rkt"
         "zcpkg-query.rkt")

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

(define (validate-zcpkg-info info #:for-server? [for-server? #f])
  (define errors null)
  (define (proc->string p) (symbol->string (object-name p)))
  (define prefix-length (add1 (string-length (proc->string zcpkg-info))))

  (define (<< f . a) (set! errors (cons (apply format f a) errors)))
  (define (check predicate accessor expected)
    (unless (predicate (accessor info))
      (<< "~a: not ~a. Got: ~s"
          (substring (proc->string accessor) prefix-length)
          expected
          (accessor info))))

  (check name-string? zcpkg-info-provider-name
         "a valid name string")

  (check name-string? zcpkg-info-package-name
         "a valid name string")

  (check name-string? zcpkg-info-edition-name
         "a valid name string")

  (check revision-number? zcpkg-info-revision-number
         "an exact non-negative integer")

  (check (listof name-string?) zcpkg-info-revision-names
         "a list of name strings")

  (check (or/c #f path-string?)
         zcpkg-info-setup-module
         "a path string, or #f")

  (check (listof (or/c zcpkg-query-string?
                       path-string?
                       url-string?))
         zcpkg-info-dependencies
         "a list containing zcpkg-query URNs, URLs, or paths")

  (when for-server?
    (check non-empty-bytes? zcpkg-info-integrity
           "a byte string")

    (check non-empty-bytes? zcpkg-info-signature
           "a byte string"))

  (reverse errors))


(define (integrous-artifact? artifact-path info)
  (or (equal? (make-digest artifact-path)
              (zcpkg-info-integrity info))
      (ZCPKG_TRUST_BAD_DIGEST)))

(define (authenticated-provider? info public-key)
  (define signature
    (zcpkg-info-signature info))

  (if signature
      (or (verify-signature (zcpkg-info-integrity info)
                            signature
                            public-key)
          (ZCPKG_TRUST_BAD_SIGNATURE))
      (ZCPKG_TRUST_UNSIGNED)))
