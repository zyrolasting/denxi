#lang racket/base

(provide recant-command)

(require racket/cmdline
         "../url.rkt"
         "../service/endpoint.rkt"
         "../dependency.rkt")

(define warning-fmt-string #<<EOF
Recanting an artifact deletes it and all non-identifying metadata.
The package identity will remain, which means that users will see that
a requested artifact is no longer available.

Are you absolutely sure that you want to do this?

If so, then there is no going back.

Type the artifact name again to confirm.

EOF
)


(define (recant-command)
  (define example "example.com/package;my-edition;0")

  (command-line #:program "recant"
                #:args (dependency-string)
                (define dep
                  (with-handlers ([exn?
                                   (λ (e)
                                     (printf "Could not parse ~s as a dependency string.~n"
                                             dependency-string)
                                     (printf "You need to specify an exact version like ~s~n"
                                             example)
                                     (printf "Error: ~a~n" (exn-message e))
                                     (exit 1))])
                    (string->dependency dependency-string)))

                (unless (dependency-exact? dep)
                  (printf "Cannot recant ~s. You need to specify an EXACT version.~n" dependency-string)
                  (printf "This helps protect you from recanting the wrong thing.~n")
                  (printf "e.g. ~s~n" example)
                  (exit 1))

                (printf warning-fmt-string)
                (printf "Type ~s (without quotes) to recant, or 'q' to quit.~n" dependency-string)
                (let loop ()
                  (display ">> ")
                  (flush-output)
                  (define input (read-line))
                  (cond [(equal? input dependency-string)
                         'continue]
                        [(equal? input "q")
                         (exit 0)]
                        [else
                         (displayln "Input does not match. Please try again.")
                         (loop)]))))
