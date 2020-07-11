#lang racket/base

(provide (all-defined-out))

(require racket/port
         "string.rkt"
         "config.rkt")

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

(define (prompt/confirmation message
                             #:param param
                             #:default [default 'no]
                             #:dangerous? [dangerous? #t])
  (define (tag-default m v)
    (~a m (if (eq? default v) " (default)" "")))

  (define out
    (if (eq? (param) 'ask)
        (prompt
         (~a* (if dangerous? "/!\\ WARNING /!\\" "")
              message
              (tag-default "n - No" 'no)
              (tag-default "y - Yes" 'yes)
              (tag-default "a - Always yes" 'always))
         read-char
         (λ (answer)
           (case answer
             [(#\newline) default]
             [(#\n #\N) 'no]
             [(#\y #\Y) 'yes]
             [(#\a #\A) 'always]
             [else #f])))
        (param)))

  ; A yes only applies once.
  (param (if (eq? out 'yes) 'ask out))

  out)


(define (prompt/use-installer name)
  (prompt/confirmation #:dangerous? #f
                       #:param ZCPKG_USE_INSTALLER
                       (~a "Do you want to run " name "'s installer?")))
