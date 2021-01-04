#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    xiden/cmdline
                    xiden/cli
                    xiden/cli-flag
                    xiden/logged
                    xiden/message
                    xiden/rc
                    xiden/workspace]
          "../../shared.rkt"]

@title{Command Line Argument Parsers}

@defmodule[xiden/cli]

@racketmodname[xiden/cli] implements the command line parsers for
Xiden in terms of @racketmodname[xiden/cmdline]. Each parser
prepares a program that uses continuation passing style to return
output and an exit code.

@racket[(submod xiden/cli main)] is the entry point for the
@litchar{xiden} and @litchar{raco zcpkg} commands.

@defthing[top-level-cli argument-parser/c]{
Returns a program based on the first argument, @racketid[subcommand],
with consideration to command line flags that apply to every possible
subcommand. If @racketid[subcommand] is not defined, then the program
halts with exit code @racket[1] and output
@racket[($cli:undefined-command subcommand)].
}

@defthing[do-command argument-parser/c]{
Creates a program that carries out work defined in @racket[args]. In
the event no work is possible, then the program will trivially
halt with exit code 0 and produce no output.

Otherwise, the command will build a transaction where command-line
flags add work to execute in reading order.
}


@defthing[gc-command argument-parser/c]{
Creates a program that collects garbage in a @tech{target workspace}.

Assuming no exceptional behavior, the bound program halts with exit code 0
with output @racket[($finished-collecting-garbage (xiden-collect-garbage))].
}


@defthing[show-command argument-parser/c]{
@racket[show-command] creates a program with behavior based on the
first argument @racket[A].

If @racketid[A] is @racket{config}, the program halts with exit code 0
and output @racket[($show-datum (dump-xiden-settings))].

If @racketid[A] is @racket{installed}, the program halts with exit
code 0 and output @racket[(list ($show-string S) ...)], where
@racket[S] is a string formatted to show a @tech{package query}, a
package output and a directory path. The list represents all installed
outputs in the @tech{target workspace}.

If @racket{links}, the program halts with exit code 0 and output
@racket[(list ($show-string L) ...)], where @racket[L] is a string
containing a path to a symbolic link and a path to another filesystem
entry referenced by that link. These symbolic links are special in
that they are tracked by Xiden for garbage collection
purposes.

If @racket{workspace}, the program halts with exit code 0
and output @racket[($show-string (path->string (workspace-directory)))].

In all other cases, the program halts with exit code 1 and output
@racket[($cli:undefined-command A)].

}
