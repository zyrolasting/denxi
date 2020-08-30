#lang racket/base

(require "contract.rkt")
(provide
 with-rc
 (contract-out
  [run-command-line
   (->* ((unconstrained-domain-> (integer-in 0 255))
         #:program non-empty-string?
         #:arg-help-strings (listof non-empty-string?))
        (#:flags list?
         #:args (or/c (vectorof string?) (listof string?))
         string?)
        (integer-in 0 255))]))

(require racket/cmdline
         racket/vector
         "message.rkt"
         "monad.rkt"
         "printer.rkt"
         "rc.rkt"
         "setting.rkt"
         "string.rkt"
         "workspace.rkt")

(define+provide-message $unrecognized-command (command))

; Define a transition from accumulated command line flags to a new
; parameterization in terms of those flags and a cached read of the
; rcfile. Capture any failure in this transition as main program
; output.
(define-syntax-rule (with-rc flags body ...)
  (with-handlers ([exn:fail? (λ (e) (attach-message 0 ($show-string (exn-message e))))])
    (with-xiden-rcfile (call-with-applied-settings flags (λ () body ...)))))


; Base bindings follow

(define (run-command-line #:program program
                          #:flags [flags null]
                          #:args [args (current-command-line-arguments)]
                          #:arg-help-strings arg-help-strings
                          #:suffix-is-index? [suffix-is-index? #t]
                          handle-arguments
                          [help-suffix ""])
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
    (attach-message 1
     ($show-string
      (format "~a~n~a"
              (exn-message e)
              (if (and (regexp-match? #px"given 0 arguments" (exn-message e))
                       suffix-is-index?
                       (not help-requested?))
                  help-suffix
                  "")))))


  (call/cc
   ; The callback for showing the help string does not stop evaluation
   ; of the argument handler. This is why parse-command-line calls the
   ; exit handler by default. Use a continuation to maintain a
   ; functional approach.
   (λ (force-output)
     (with-handlers ([exn:fail:user? show-help-on-zero-arguments])
       (parse-command-line program argv
                           (if (null? flags)
                               null
                               `((once-each . ,flags)))
                           handle-arguments
                           arg-help-strings
                           (λ (help-str)
                             (force-output
                              (return (attach-message (if help-requested? 0 1)
                                                      ($show-string (format "~a~n~a" help-str help-suffix)))))))))))
