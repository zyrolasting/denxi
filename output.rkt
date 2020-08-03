#lang racket/base

; Define program output

(require racket/fasl
         racket/pretty
         racket/serialize
         "contract.rkt"
         "format.rkt"
         "message.rkt"
         "zcpkg-settings.rkt"
         "zcpkg-messages.rkt")

(provide
 (contract-out
  [write-output (->* ($message?) (output-port?) void?)]))

(define important-messages
  (list $already-installed?
        $on-compilation-error?
        $on-bad-digest?
        $on-bad-signature?
        $on-missing-signature?
        $on-unverified-host?
        $on-package-installed?
        $install-package?))

(define (include-output? m)
  (or (ZCPKG_VERBOSE)
      (ormap (λ (?) (? m)) important-messages)))

(define (write-output v [out (current-output-port)])
  (when (include-output? v)
    (parameterize ([current-output-port out])
      (define to-send
        (if (ZCPKG_READER_FRIENDLY_OUTPUT)
            v
            (format-zcpkg-message v)))

      (if (ZCPKG_FASL_OUTPUT)
          (s-exp->fasl (serialize to-send) (current-output-port))
          (if (ZCPKG_READER_FRIENDLY_OUTPUT)
              (pretty-write #:newline? #t to-send)
              (displayln to-send)))
      (flush-output))))

(module+ test
  (require rackunit
           "setting.rkt"
           "zcpkg-info.rkt"
           (submod "zcpkg-info.rkt" test))

  (define (capture-bytes p)
    (define buffer (open-output-bytes))
    (p buffer)
    (get-output-bytes buffer #t))

  (define (test-output msg v expected)
    (define buffer (open-output-bytes))
    (write-output v buffer)
    (test-true msg
               (if (or (regexp? expected)
                       (pregexp? expected))
                   (regexp-match? expected (get-output-bytes buffer #t))
                   (equal? (get-output-bytes buffer #t) expected))))

  (test-output "By default, program output is human-friendly"
               ($already-installed dummy-zcpkg-info)
               #px"already installed at")

  (assume-settings ([ZCPKG_READER_FRIENDLY_OUTPUT #t])
    (test-output "Allow reader-friendly output"
                 ($already-installed dummy-zcpkg-info)
                 (capture-bytes
                  (λ (o)
                    (pretty-write #:newline? #t ($already-installed dummy-zcpkg-info) o)))))

  (assume-settings ([ZCPKG_READER_FRIENDLY_OUTPUT #t]
                    [ZCPKG_FASL_OUTPUT #t])
    (test-case "Allow FASL output"
      (define in
        (open-input-bytes
         (capture-bytes
          (λ (o) (write-output ($already-installed dummy-zcpkg-info) o)))))

      (check-equal? (deserialize (fasl->s-exp in))
                    ($already-installed dummy-zcpkg-info))))

  (test-case "Control verbose output"
    (assume-settings ([ZCPKG_VERBOSE #f])
                     (test-output "Opt out of verbose output"
                                  ($fail 1)
                                  #""))

    (assume-settings ([ZCPKG_VERBOSE #t]
                      [ZCPKG_READER_FRIENDLY_OUTPUT #t])
                     (test-output "Opt into verbose output"
                                  ($fail 1)
                                  #px"\\$fail"))))
