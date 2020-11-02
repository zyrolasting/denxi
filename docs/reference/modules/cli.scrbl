#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    xiden/cmdline
                    xiden/cli
                    xiden/logged
                    xiden/message
                    xiden/rc]
          "../../shared.rkt"]

@title{Command Line Interface}

@defmodule[xiden/cli]

@racketmodname[xiden/cli] implements the command line handlers for
@|project-name|. Each handler uses continuation passing style to
return program output and an exit code.

@racket[(submod xiden/cli main)] is the entry point for the
@litchar{xiden} and @litchar{raco zcpkg} commands. It only calls
the default @tech/reference{exit handler} using the output
of @racket[entry-point] like so:

@racketblock[
(module+ main
  (exit (entry-point (current-command-line-arguments)
                     (get-message-formatter)
                     top-level-cli)))]

@defthing[after-program/c chaperone-contract? #:value (-> exit-code/c messy-log/c exit-code/c)]{
After a program finishes running, an @racket[after-program/c]
procedure is called for its effect.  In normal operation, it must
return the first argument, the exit code, unchanged. The
@tech{messages} passed as the second argument represent a program log.

The behavior of this function may vary for blackbox testing.
}


@defproc[(top-level-cli [args arguments/c] [halt after-program/c]) exit-code/c]{
The first argument, @racketid[subcommand], is used to
select another procedure.  If there is no defined command based on the
first argument, then @racket[top-level-cli] returns @racket[(halt 1
($cli:undefined-command subcommand))]. Otherwise, it returns
the result of that procedure applied to the remaining arguments
and @racket[halt].

The subcommand procedure runs in a @tech/reference{parameterization}
with possibly overridden values for:

@itemlist[
@item{@racket[XIDEN_FASL_OUTPUT]}
@item{@racket[XIDEN_READER_FRIENDLY_OUTPUT]}
@item{@racket[XIDEN_VERBOSE]}
]
}

@defproc[(do-command [args arguments/c] [halt after-program/c]) exit-code/c]{
Runs a transaction with a scope of work set by @racket[args]. In the
event no work is possible, then the command will trivially succeed
with no additional output (meaning @racket[(halt 0 null)]).

Otherwise, the command will build a transaction where command-line
flags add work to execute in reading order.

}


@defproc[(gc-command [args arguments/c] [halt after-program/c]) exit-code/c]{
Collects garbage in a @tech{target workspace}.

Returns @racket[(halt 0 ($finished-collecting-garbage (xiden-collect-garbage)))].
}


@defproc[(show-command [args arguments/c] [halt after-program/c]) exit-code/c]{
@racket[show-command] accepts no flags. The first argument @racket[A] determines its behavior:

If no case shown below fits, then @racket[show-command] returns
@racket[(halt 1 ($cli:undefined-command A))].

If @racketid[A] is @racket{config}, @racket[show-command] returns
@racket[(halt 0 ($show-datum (dump-xiden-settings)))].

If @racket{installed}, @racket[show-command] returns @racket[(halt 0
(list ($show-string S) ...))], where @racket[S] is a string formatted
to show a @tech{package query}, a package output and a directory
path. The list represents all installed outputs in the @tech{target
workspace}.

If @racket{links}, @racket[show-command] returns @racket[(halt 0 (list
($show-string L) ...))], where @racket[L] is a string that shows a
path to a symbolic link and a path to another filesystem entry
referenced by that link. These symbolic links are special in that they
are tracked by @project-name for garbage collection purposes.

If @racket{workspace}, @racket[show-command] returns @racket[(halt 0
($show-string (path->string (workspace-directory))))].

}
