#lang racket/base

; Write $message instances to ports

(require "contract.rkt")

(provide define-message-formatter
         (contract-out
          [default-message-formatter
            (-> $message? string?)]
          [write-message
           (->* ($message? (-> $message? string?)) (output-port?) void?)]
          [mwrite-message
           (->* ($message? (-> $message? string?)) (output-port?) io-return?)]))

(require racket/date
         racket/fasl
         racket/match
         racket/pretty
         racket/serialize
         "message.rkt"
         "monad.rkt"
         "rc.rkt")

(define+provide-message $show-datum  (value))
(define+provide-message $show-string (message))
(define+provide-message $verbose     (message))


(define (filter-output m)
  (if ($verbose? m)
      (and (XIDEN_VERBOSE)
           ($verbose-message m))
      m))


(define (mwrite-message v formatter [out (current-output-port)])
  (io-return (λ () (write-message v formatter out))))


(define (write-message v formatter [out (current-output-port)])
  (define maybe-message (filter-output v))
  (when maybe-message
    (parameterize ([current-output-port out])
      (define to-send
        (if (XIDEN_READER_FRIENDLY_OUTPUT)
            maybe-message
            (formatter maybe-message)))

      (if (XIDEN_FASL_OUTPUT)
          (s-exp->fasl (serialize to-send) (current-output-port))
          (if (XIDEN_READER_FRIENDLY_OUTPUT)
              (pretty-write #:newline? #t to-send)
              (displayln to-send)))
      (flush-output))))


(define-syntax-rule (define-message-formatter id patts ...)
  (define (id m)
    (match m patts ... [_ (error 'id "Unknown message type: ~s" m)])))


(define-message-formatter default-message-formatter
  [($show-string v) v]
  [($show-datum v) (pretty-format #:mode 'write v)])


(module+ test
  (require rackunit
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

  (XIDEN_READER_FRIENDLY_OUTPUT #t
    (λ ()
      (test-output "Allow reader-friendly output"
                   dummy
                   (capture-bytes
                    (λ (o)
                      (pretty-write #:newline? #t dummy o))))

      (XIDEN_FASL_OUTPUT #t
        (λ ()
          (test-case "Allow FASL output"
            (define in
              (open-input-bytes
               (capture-bytes
                (λ (o) (write-output dummy o)))))

            (check-equal? (deserialize (fasl->s-exp in))
                          dummy))))))

  (test-case "Control verbose output"
    (XIDEN_VERBOSE #f
      (λ ()
        (test-output "Opt out of verbose output"
                     ($verbose dummy)
                     #"")))

    (parameterize ([(setting-derived-parameter XIDEN_VERBOSE) #t]
                   [(setting-derived-parameter XIDEN_READER_FRIENDLY_OUTPUT) #t])
      (test-output "Opt into verbose output"
                   ($verbose dummy)
                   #px"\\$show-string"))))
