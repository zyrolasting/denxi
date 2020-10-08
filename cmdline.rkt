#lang racket/base

(require "contract.rkt")

(define exit-code/c (integer-in 0 255))
(define arguments/c (or/c (vectorof string?) (listof string?)))
(define program-log/c (or/c $message? (listof $message?)))

(provide
 with-rc
 run-command-line
 (contract-out
  [exit-code/c flat-contract?]
  [arguments/c chaperone-contract?]
  [program-log/c chaperone-contract?]
  [entry-point
   (-> arguments/c
       message-formatter/c
       (-> arguments/c
           (-> exit-code/c program-log/c any)
           (values exit-code/c program-log/c))
       exit-code/c)]))

(require racket/cmdline
         racket/list
         racket/format
         racket/vector
         "cli-flag.rkt"
         "message.rkt"
         "monad.rkt"
         "printer.rkt"
         "rc.rkt"
         "setting.rkt"
         "string.rkt"
         "workspace.rkt")


(define+provide-message $cli $message ())
(define+provide-message $cli:undefined-command $cli (command))
(define+provide-message $cli:show-help $cli (body-string suffix-string-key))

(define (entry-point args format-message run-args)
  (run-io (make-entry-point args format-message run-args)))

; A functional (as in "functional programming") entry point to the
; project. Use for functional (as in "black box") tests.
;
; The code makes this obvious, but it's easy to forget that the I/O
; marked with * is only meant for an end of program report.  Do not
; count on them to include download progress, etc.
(define (make-entry-point args format-message run-args)
  (do (if (show-workspace-envvar-error?)
          (mwrite-message ($invalid-workspace-envvar))
          (io-return void))
      (let-values ([(exit-code messages) ; CPS is easier to think about in the handlers.
                    (call/cc (λ (k) (run-args args k)))])
        (do (io-return ; *
             (λ () (for ([m (if (list? messages) (in-list (reverse (flatten messages))) (in-value messages))])
                     (write-message m format-message))))
            (return exit-code)))))

; Define a transition from accumulated command line flags to a new
; parameterization in terms of those flags and a cached read of the
; rcfile. Capture any failure in this transition as main program
; output.
(define-syntax-rule (with-rc flags body ...)
  (with-handlers ([exn:fail? (λ (e) (values 0 ($show-string (exn-message e))))])
    (with-xiden-rcfile (call-with-bound-cli-flags flags (λ () body ...)))))


; Base bindings follow

(define (run-command-line #:program program
                          #:halt halt
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

  (define help-requested?
    (or (vector-member "-h" argv)
        (vector-member "--help" argv)))

  ; parse-command-line does not show help when arguments are missing
  ; and -h is not set.  Show help anyway.
  (define (show-help-on-zero-arguments e)
    (halt 1
          ($cli:show-help (exn-message e)
           (and (and (regexp-match? #px"given 0 arguments" (exn-message e))
                     (not help-requested?))
                help-suffix-string-key))))

  (with-handlers ([exn:fail:user? show-help-on-zero-arguments])
    (parse-command-line program argv
                        flags
                        handle-arguments
                        arg-help-strings
                        (λ (help-str)
                          (halt (if help-requested? 0 1)
                                ($cli:show-help help-str help-suffix-string-key))))))
