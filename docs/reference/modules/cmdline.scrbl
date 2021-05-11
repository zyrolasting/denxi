#lang scribble/manual

@require["../../shared.rkt"
         @for-label[racket/base
                    racket/contract
                    racket/cmdline
                    xiden/cli
                    xiden/cli-flag
                    xiden/cmdline
                    xiden/format
                    xiden/l10n
                    xiden/subprogram
                    xiden/message
                    xiden/printer]]

@title{Command Line Utilities}

@defmodule[xiden/cmdline]

@racketmodname[xiden/cmdline] provides all bindings from
@racketmodname[racket/cmdline], as well as the bindings documented in
this section. Unlike @racketmodname[xiden/cli],
@racketmodname[xiden/cmdline] does not actually define a command line
interface. It only helps @racketmodname[xiden/cli] do so.

@section{CLI Value Types}

@defthing[exit-code/c flat-contract? #:value (integer-in 0 255)]{
An exit code for use in an @tech/reference{exit handler}.

Xiden does not currently lean on the exit code to convey much
meaning, so expect to see @racket[1] (@tt{E_FAIL}) to represent an
error state. Lean on the @tech{program log} for specifics.
}

@defthing[exit-handler/c contract? #:value (-> exit-code/c any)]{
A procedure that reacts to an exit code, like @racket[exit].
}

@defthing[bound-program/c contract? #:value (-> (-> exit-code/c (or/c $message? subprogram-log/c) any) any)]{
A procedure meant to run predetermined program logic in the context of
a @tech{runtime configuration}.

The only argument is a procedure (probably representing a
@tech/reference{continuation}) that accepts an exit code and a
@tech{subprogram log} representing program output. That continuation
will decide how to react to the given information.
}

@defthing[arguments/c chaperone-contract? #:value (or/c (listof string?) (listof vector?))]{
Represents arguments provided in a command line. An argument list may
be a vector or list of strings for flexibility reasons.
}

@defthing[argument-parser/c contract? #:value (-> arguments/c (values (listof cli-flag-state?) bound-program/c))]{
A procedure that parses command line arguments and plans further action.
}

@defthing[program-log/c chaperone-contract? #:value (or/c $message? (listof $message?))]{
A @deftech{program log} is a single @tech{message} or list of
@tech{messages} that report exact program behavior. The list is sorted
such that the first element is the first @tech{message} to report in
the respective program.
}

@section{CLI Flow Control}

@defproc[(run-entry-point! [args arguments/c]
                           [formatter message-formatter/c]
                           [parse-args argument-parser/c]
                           [on-exit exit-handler/c])
                           any]{
This translates the given command-line arguments to a program to run
in the relevant @tech{runtime configuration}, and applies
@racket[on-exit] to the exit code of the process.

This module mimics production behavior.

@racketblock[
(module+ main
  (run-entry-point! (current-command-line-arguments)
                    (get-message-formatter)
                    top-level-cli
                    exit))]
}



@defproc[(cli [#:program program string?]
              [#:flags flags list? null]
              [#:args args (or/c (vector/c string?) (listof string?)) (current-command-line-arguments)]
              [#:help-suffix-string-key help-suffix-string-key (or/c #f symbol?) #f]
              [#:arg-help-strings arg-help-strings (listof string?)]
              [handle-arguments (->* ((listof cli-flag-state?)) () #:rest list?
                                (values (listof cli-flag-state?)
                                        bound-program/c))])
         (values (listof cli-flag-state?)
                 bound-program/c)]{
Returns captured command line flags and a procedure representing the
program's behavior.

First, @racket[cli] applies the following expression:

@racketblock[
(parse-command-line program args flags handle-arguments arg-help-strings handle-help)]

Note that some arguments used as-is. Others are computed towards the
same ends.

@racket[handle-help] adds localized contextual help using
@racket[help-suffix-string-key]. The exit code from such a program is
@racket[0] if the user actually requested help in
@racket[args]. @racket[1] otherwise. The same help suffix is used in
the event the user does not pass enough positional arguments for
@racket[handle-arguments].

The following example shows the relationship between command line
parsing, a @tech{runtime configuration} based on flags, and actual
program execution.

@racketblock[
(define (say-hello . args)
  (cli #:program "hi"
       #:flags null
       #:args args
       #:arg-help-strings '("names")
       (lambda (flags . names)
         (values flags
                 (lambda (halt)
                    (halt 0
                          (for/list ([name names])
                            ($show-string (format "Hello, ~a!" name)))))))))


(define-values (flags program) (say-hello "john" "sage" "mary"))
(define message->string (get-message-formatter))

(call-with-bound-cli-flags flags
  (lambda ()
    (program (lambda (exit-code messages)
      (printf "Exit code: ~a~n" exit-code)
      (for ([m messages])
        (write-message m message->string))))))
]

}



@section{CLI Messages}

@defstruct*[($cli $message) () #:prefab]{
A @tech{message} that pertains to a command line interface.
}

@defstruct*[($cli:undefined-command $cli) ([command string?])  #:prefab]{
A user passed @racket[command] as a subcommand, but it is not defined.
}

@defstruct*[($cli:show-help $cli) ([body-string string?] [string-suffix-key (or/c #f symbol?)])  #:prefab]{
A @tech{message} that formats as context-sensitive
help. @racket[body-string] is help content generated by
@racket[parse-command-line]. @racket[string-suffix-key] is an internal
value used to select a localized string to append to said help.
}
