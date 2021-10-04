#lang racket/base

; Write $message instances to ports

(require racket/contract
         racket/fasl
         racket/list
         racket/pretty
         racket/serialize
         "format.rkt"
         "message.rkt"
         "setting.rkt"
         "subprogram.rkt")


(provide (contract-out
          [write-message-log
           (-> (or/c $message? subprogram-log/c)
               message-formatter/c
               void?)]
          [write-message
           (->* ($message?) (#:newline? any/c message-formatter/c output-port?) void?)]))

(define+provide-message $verbose (message))
(define+provide-setting DENXI_FASL_OUTPUT boolean? #f)
(define+provide-setting DENXI_VERBOSE boolean? #f)
(define+provide-setting DENXI_READER_FRIENDLY_OUTPUT boolean? #f)

(define (filter-output m)
  (if ($verbose? m)
      (and (DENXI_VERBOSE)
           ($verbose-message m))
      m))


; Program output can be a subprogram log so that users don't always have to
; construct an organized list of messages.
(define (write-message-log program-output format-message)
  (define messages
    (if (list? program-output)
        (reverse (flatten program-output))
        (in-value program-output)))
  (for ([m messages])
    (write-message m format-message)))


(define (write-message v
                       #:newline? [newline? #t]
                       [formatter (current-message-formatter)]
                       [out (current-output-port)])
  (define maybe-message (filter-output v))
  (when maybe-message
    (parameterize ([current-output-port out])
      (define to-send
        (if (DENXI_READER_FRIENDLY_OUTPUT)
            maybe-message
            (formatter maybe-message)))

      (if (DENXI_FASL_OUTPUT)
          (s-exp->fasl (serialize to-send) (current-output-port))
          (if (DENXI_READER_FRIENDLY_OUTPUT)
              (pretty-write #:newline? newline? to-send)
              ((if newline? displayln display) to-send)))
      (flush-output))))

(module+ test
  (require racket/format
           rackunit
           "setting.rkt")

  (define dummy ($show-string "Testing: Blah"))

  (define (write-output v [out (current-output-port)])
    (write-message v default-message-formatter out))

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
               dummy
               #px"Testing: Blah")

  (DENXI_READER_FRIENDLY_OUTPUT #t
    (λ ()
      (test-output "Allow reader-friendly output"
                   dummy
                   (capture-bytes
                    (λ (o)
                      (pretty-write #:newline? #t dummy o))))

      (DENXI_FASL_OUTPUT #t
        (λ ()
          (test-case "Allow FASL output"
            (define in
              (open-input-bytes
               (capture-bytes
                (λ (o) (write-output dummy o)))))

            (check-equal? (deserialize (fasl->s-exp in))
                          dummy))))))

  (test-case "Control verbose output"
    (DENXI_VERBOSE #f
      (λ ()
        (test-output "Opt out of verbose output"
                     ($verbose dummy)
                     #"")))

    (DENXI_VERBOSE #t
      (λ () (DENXI_READER_FRIENDLY_OUTPUT #t
        (λ ()
          (test-output "Opt into verbose output"
                       ($verbose dummy)
                       #px"\\$show-string")))))))
