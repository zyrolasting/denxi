#lang racket/base

; Define procedures to write program output to ports

(require racket/contract)
(provide
 (contract-out
  [write-output
   (->* ($message?) (output-port?) void?)]))

(require racket/fasl
         racket/pretty
         racket/serialize
         "format.rkt"
         "message.rkt"
         "rc.rkt"
         "xiden-messages.rkt")


(define (filter-output m)
  (if ($verbose? m)
      (and (XIDEN_VERBOSE)
           ($verbose-message m))
      m))


(define (write-output v [out (current-output-port)])
  (define maybe-message (filter-output v))
  (when maybe-message
    (parameterize ([current-output-port out])
      (define to-send
        (if (XIDEN_READER_FRIENDLY_OUTPUT)
            maybe-message
            (format-xiden-message maybe-message)))

      (if (XIDEN_FASL_OUTPUT)
          (s-exp->fasl (serialize to-send) (current-output-port))
          (if (XIDEN_READER_FRIENDLY_OUTPUT)
              (pretty-write #:newline? #t to-send)
              (displayln to-send)))
      (flush-output))))


(module+ test
  (require rackunit
           "setting.rkt"
           (submod "package-info.rkt" test))

  (define (capture-bytes p)
    (define buffer (open-output-bytes))
    (p buffer)
    (get-output-bytes buffer #t))

  (define (test-output msg v expected)
    (define buffer (open-output-bytes))

    ; parameterize adds coverage for default value in write-output
    (parameterize ([current-output-port buffer])
      (write-output v))

    (test-true msg
               (if (or (regexp? expected)
                       (pregexp? expected))
                   (regexp-match? expected (get-output-bytes buffer #t))
                   (equal? (get-output-bytes buffer #t) expected))))

  (test-output "By default, program output is human-friendly"
               ($already-installed dummy-package-info)
               #px"already installed")

  (XIDEN_READER_FRIENDLY_OUTPUT #t
    (λ ()
      (test-output "Allow reader-friendly output"
                   ($already-installed dummy-package-info)
                   (capture-bytes
                    (λ (o)
                      (pretty-write #:newline? #t ($already-installed dummy-package-info) o))))

      (XIDEN_FASL_OUTPUT #t
        (λ ()
          (test-case "Allow FASL output"
            (define in
              (open-input-bytes
               (capture-bytes
                (λ (o) (write-output ($already-installed dummy-package-info) o)))))

            (check-equal? (deserialize (fasl->s-exp in))
                          ($already-installed dummy-package-info)))))))

  (test-case "Control verbose output"
    (XIDEN_VERBOSE #f
      (λ ()
        (test-output "Opt out of verbose output"
                     ($verbose ($stop))
                     #"")))

    (parameterize ([(setting-derived-parameter XIDEN_VERBOSE) #t]
                   [(setting-derived-parameter XIDEN_READER_FRIENDLY_OUTPUT) #t])
      (test-output "Opt into verbose output"
                   ($verbose ($stop))
                   #px"\\$stop"))))
