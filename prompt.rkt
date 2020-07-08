#lang racket/base

(require idiocket/format
         idiocket/port
         "string.rkt")

(define (~a* . args)
  (apply ~a (map (λ (s) (~a s "\n")) args)))

(define (purge-ready-input . _)
  (and (byte-ready?)
       (purge-ready-input (read-byte))))

(define (prompt message read-answer consider-answer)
  (displayln message)
  (let loop ()
    (purge-ready-input)
    (display "> ")
    (flush-output)
    (define answer (read-answer (sync/enable-break (current-input-port))))
    (define control
      (if (eof-object? answer)
          answer
          (consider-answer answer)))
    (cond [(eof-object? answer)
           (error 'prompt "Unexpected EOF in prompt")]
          [(string? control)
           (displayln control)
           (loop)]
          [control]
          [else (printf "Unrecognized input: ~s.~n" answer)
                (displayln "Please check the prompt and try again.")
                (loop)])))

(define (consider-yes/no/always answer)
  (case answer
    [(#\n #\N) 'fail]
    [(#\y #\Y) 'yes-once]
    [(#\a #\A) 'yes-always]
    [else #f]))


(define (report-dependencies dependencies dependent-name)
  (prompt (~a* dependent-name
               " needs the following to work: "
               (string-join (map (λ (s) (~a "  " s)) dependencies) "\n")
               "Install these too?"
               "n - No (default)"
               "y - Yes"
               "a - Yes, and don't ask again.")
          read-char
          consider-yes/no/always))
