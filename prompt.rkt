#lang racket/base

(provide prompt
         prompt/confirmation
         prompt/new-dependencies
         prompt/signature-missing
         prompt/signature-mismatch
         prompt/integrity-violation
         prompt/use-installer
         prompt/uninstall-dependencies)


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

(define (prompt/confirmation message #:default [default 'no] #:dangerous? [dangerous? #t])
  (define (tag-default m v)
    (~a m (if (eq? default v) " (default)" "")))
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
       [else #f]))))


(define (prompt/new-dependencies dependencies dependent-name)
  (prompt/confirmation #:dangerous? #f
                       (~a* (~a dependent-name " needs the following to work: ")
                            (string-join (map (λ (s) (~a "  " s)) dependencies) "\n")
                            "Install these too?")))

(define (prompt/signature-missing name)
  (prompt/confirmation
   (~a* (~a name " is unsigned.")
        "If you are testing your own package, you can safely proceed."
        "Otherwise, proceeding means running code from an unverified source."
        "Do you want to install this package anyway?")))

(define (prompt/signature-mismatch name)
  (prompt/confirmation
   (~a* (~a name "'s signature does not match the provider's key.")
        "If you are testing your own package, you can safely proceed."
        "Otherwise, proceeding means running code from an unverified source."
        "Do you want to install this package anyway?")))

(define (prompt/integrity-violation name)
  (prompt/confirmation
   (~a* (~a name " may have been corrupted or altered.")
        "Do you want to install this package anyway?")))

(define (prompt/use-installer name)
  (prompt/confirmation #:dangerous? #f
                       (~a "Do you want to run " name "'s installer?")))

(define (prompt/uninstall-dependencies name affected)
  (prompt/confirmation #:dangerous? #f
                       (~a* (~a "Uninstalling " name " will break the following dependent packages.")
                            (string-join (map (λ (s) (~a "  " s)) affected) "\n")
                            "Do you want to uninstall them too?")))

(module+ test
  (provide run-all-prompts)
  (require rackunit)

  (define (run-all-prompts)
    (define name "some-package")
    (prompt/new-dependencies '("stdlib" "framework")
                             name)
    (prompt/signature-missing name)
    (prompt/signature-mismatch name)
    (prompt/integrity-violation name)
    (prompt/use-installer name)
    (prompt/uninstall-dependencies name '("dependent-pkg" "thing-you-forgot-about"))))
