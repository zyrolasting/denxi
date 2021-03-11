#lang racket/base

; Extend racket/cmdline. cli.rkt defines the CLI in terms of this
; module.

(require racket/cmdline
         racket/vector
         "cli-flag.rkt"
         "contract.rkt"
         "format.rkt"
         "logged.rkt"
         "message.rkt"
         "printer.rkt"
         "rc.rkt"
         "setting.rkt"
         "workspace.rkt")

(define exit-code/c (integer-in 0 255))
(define exit-handler/c (-> exit-code/c any))
(define arguments/c (or/c (vectorof string?) (listof string?)))
(define program-log/c (or/c $message? (listof $message?)))
(define bound-program/c (-> (-> exit-code/c messy-log/c any) any))
(define argument-parser/c
  (-> arguments/c
      (values (listof cli-flag-state?)
              bound-program/c)))


(provide cli
         (contract-out
          [exit-code/c flat-contract?]
          [exit-handler/c contract?]
          [arguments/c contract?]
          [bound-program/c contract?]
          [program-log/c contract?]
          [argument-parser/c contract?]
          [run-entry-point!
           (-> arguments/c
               message-formatter/c
               argument-parser/c
               exit-handler/c
               any)]))

(define+provide-message $cli $message ())
(define+provide-message $cli:undefined-command $cli (command))
(define+provide-message $cli:show-help $cli (body-string suffix-string-key))


; This procedure ties together all of the functional parts of the program
; into a single procedure called for its effect.
(define (run-entry-point! args format-message parse-args on-exit)
  (on-exit
   (call/cc
    (λ (use-exit-code)
      ; TODO: An improperly configured workspace directory should be
      ; cause to halt the program.
      (when (show-workspace-envvar-error?)
        (write-message ($invalid-workspace-envvar) format-message))

      (with-handlers ([exn? (λ (e)
                              (write-message ($show-string (exn-message e)) format-message)
                              (use-exit-code 1))]
                      [$message?
                       (λ (m)
                         (write-message m format-message)
                         (use-exit-code 1))])
        (define-values (flags run!) (parse-args args))
        (call-with-applied-settings
         (build-runtime-configuration flags)
         (λ ()
           (define-values (exit-code program-output)
             (parameterize ([current-message-formatter format-message])
               (call/cc run!)))
           (write-message-log program-output format-message)
           (use-exit-code exit-code))))))))


; Capture the runtime configuration once so that it can be reused
; for more than just a command line handler.
(define (build-runtime-configuration flags)
  (call-with-bound-cli-flags
   flags
   (λ ()
     (for/hash ([(sym val) (dump-xiden-settings)])
       (values (hash-ref XIDEN_SETTINGS sym)
               val)))))




; Unlike normal use of (command-line) or (parse-command-line), (cli)
; does not actually run a program. It instead returns two values:
; Accumulated flags used to build a runtime configuration, and a
; procedure that, when applied, actually runs the program.
;
; This is useful for constructing blackbox tests in terms
; of a custom configuration.
;
; Limitation: parse-command-line's behavior depends on the arity of an
; argument handler or flag handlers. For that reason, handle-arguments
; cannot be wrapped.
;
(define (cli #:program program
             #:flags [flags null]
             #:args [args (current-command-line-arguments)]
             #:help-suffix-string-key [help-suffix-string-key #f]
             #:arg-help-strings arg-help-strings
             handle-arguments)
  ; This is helpful for functional tests since it enables vanilla quasiquoting.
  (define argv
    (if (list? args)
        (list->vector args)
        args))

  ; Whether the user expressly asked for help is important for
  ; finding error states that parse-command-line won't detect.
  (define help-requested?
    (or (vector-member "-h" argv)
        (vector-member "--help" argv)))

  (call/cc
   (λ (return)
     (define (plan-program p)
       (return null p))

     ; parse-command-line does not show help when arguments are missing
     ; and -h is not set.  Show help anyway.
     (define (show-help-on-zero-arguments e)
       (plan-program
        (λ (halt)
          (halt 1
                ($cli:show-help (exn-message e)
                                (and (and (regexp-match? #px"given 0 arguments" (exn-message e))
                                          (not help-requested?))
                                     help-suffix-string-key))))))

     ; The default help handler calls the exit handler, which doesn't
     ; make sense in this context. Instead, base whether showing help
     ; is an error state on whether the user actually asked for help.
     (define (handle-help help-str)
       (plan-program
        (λ (halt)
          (halt (if help-requested? 0 1)
                ($cli:show-help help-str help-suffix-string-key)))))

     (with-handlers ([exn:fail:user? show-help-on-zero-arguments])
       (parse-command-line program
                           argv
                           flags
                           handle-arguments
                           arg-help-strings
                           handle-help)))))

(module+ test
  (require rackunit)

  (test-case "Ask for help"
    (for ([flag (in-list '("-h" "--help"))])
      (call-with-values
       (λ () (cli #:program "help"
                  #:args (list flag)
                  #:arg-help-strings null
                  void))
       (λ (flags program)
         (check-pred null? flags)
         (program (λ (exit-code messages)
                    (check-equal? exit-code 0)
                    (check-pred $cli:show-help? messages)))))))

  (test-case "A lack of arguments will display help, but with an error code and an optional suffix"
    (call-with-values
     (λ () (cli #:program "help"
                #:args null
                #:help-suffix-string-key 'extra
                #:arg-help-strings '("v")
                (λ (flags v) v)))
     (λ (flags program)
       (check-pred null? flags)
       (program (λ (exit-code messages)
                  (check-equal? exit-code 1)
                  (check-match messages
                               ($cli:show-help (regexp "given 0 arguments")
                                               'extra)))))))

  (test-case "An argument handler can plan out a program along with updated runtime configuration"
    (call-with-values
     (λ () (cli #:program "help"
                #:args (list (shortest-cli-flag --fetch-buffer-size) "777" "x" "y")
                #:flags (make-cli-flag-table --fetch-buffer-size)
                #:arg-help-strings '("a" "b")
                (λ (flags a b)
                  (values flags (λ (halt) (halt 9 (list b a)))))))
     (λ (flags program)
       (define flag-state (car flags))
       (define bind-setting (cli-flag-state-bind flag-state))
       (bind-setting (λ () (check-equal? ((cli-flag-setting --fetch-buffer-size)) 777)))
       (program (λ (exit-code messages)
                  (check-equal? exit-code 9)
                  (check-equal? messages '("y" "x")))))))


  (test-case "Demonstrate control flow for entry point"
    (define printed-output (open-output-string))

    (define cli-args
      (list (shortest-cli-flag --fetch-buffer-size) "10" "message"))

    (define (parse-args args)
      (cli #:program "demo"
           #:args args
           #:flags (make-cli-flag-table --fetch-buffer-size)
           #:arg-help-strings '("string")
           (λ (flags str)
             (values flags
                     (λ (halt) (halt 88 ($show-string str)))))))


    (define (format-message msg)
      ; Confirm that even the message formatter has the current
      ; runtime configuration
      (check-equal? ((cli-flag-setting --fetch-buffer-size)) 10)
      (check-equal? msg ($show-string "message"))
      "written")

    (define (on-exit exit-code)
      (check-equal? exit-code 88))

    (parameterize ([current-output-port printed-output])
      (run-entry-point! cli-args
                        format-message
                        parse-args
                        on-exit))

    (check-equal? (get-output-string printed-output)
                  "written\n")))
