#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    denxi/cmdline
                    denxi/cli
                    denxi/cli-flag
                    denxi/format
                    denxi/l10n
                    denxi/state
                    denxi/subprogram
                    denxi/message
                    denxi/source]
          "../../shared.rkt"]

@title{Command Line Argument Parsers}

@defmodule[denxi/cli]

@racketmodname[denxi/cli] implements the command line parsers for
Denxi in terms of @racketmodname[denxi/cmdline]. Each parser
prepares a program that uses continuation passing style to return
output and an exit code.

@defproc[(launch-denxi! [#:arguments arguments (or/c list? vector?) (current-command-line-arguments)]
                        [#:format-message format-message message-formatter/c (get-message-formatter)]
                        [#:handle-exit handle-exit (-> any/c any) exit])
                        any]{
Returns @racket[(handle-exit status)], where @racket[status] depends
on program behavior caused as a side-effect.

The side-effect is to parse the given command line arguments, then
perform work described therein. Any @tech{messages} encountered are
printed to @racket[(current-output-port)] after being formatted by
@racket[format-message].
}


@defthing[top-level-cli argument-parser/c]{
Returns a @racket[bound-program/c] based on the first argument,
@racketid[subcommand], with consideration to command line flags that
apply to every possible subcommand. If @racketid[subcommand] is not
defined, then the program halts with exit code @racket[1] and output
@racket[($cli:undefined-command subcommand)].
}

@defthing[do-command argument-parser/c]{
Returns a @racket[bound-program/c] that carries out work defined in
@racket[args]. In the event no work is possible, then the program will
trivially halt with exit code 0 and produce no output.

Otherwise, the command will build a transaction where command-line
flags add work to execute in reading order.
}


@defthing[gc-command argument-parser/c]{
Returns a @racket[bound-program/c] that collects garbage in a
@tech{target workspace}.

Assuming no exceptional behavior, the bound program halts with exit code 0
with output @racket[($finished-collecting-garbage (denxi-collect-garbage))].
}


@defthing[show-command argument-parser/c]{
Returns a @racket[bound-program/c] with behavior based on the first
argument @racket[A].

If @racketid[A] is @racket{installed}, the program halts with exit
code 0 and output @racket[(list ($show-datum (list Q O D)) ...)],
where @racketid[Q] is a string for an @tech{exact package query},
@racketid[O] is the name of the @racket[output] from a package
defintion identified by @racketid[Q], and @racketid[D] is a directory
path containing the files from the output. All entries represent all
installed outputs in the @tech{target workspace}.

If @racketid[A] is @racket{log}, the program halts with exit code 0
and an empty program log. As a side-effect, the program prints all
data read from standard input using
@racket[(current-message-formatter)].  This is useful when one Denxi
user dumps a log to a file, and another wants to read the messages in
a different human language or notation.

If @racket{links}, the program halts with exit code 0 and output
@racket[(list ($show-datum (list L T)) ...)], where @racketid[L] is a
path to a symbolic link, and @racketid[T] is a path to another
filesystem entry referenced by that link. These symbolic links are
tracked by Denxi for garbage collection purposes, and therefore all
@racketid[T] are deleted by @racket[gc-command] when all relevant
values of @racketid[L] do not exist.

In all other cases, the program halts with exit code 1 and output
@racket[($cli:undefined-command A)].
}

@defthing[fetch-command argument-parser/c]{
Returns a @racket[bound-program/c] based on the first argument
@racketid[A].

@racketid[A] is treated as a string representation of a datum to
evaluate using @racket[eval-untrusted-source-expression]. If the
result is a @tech/denxi-reference{source} and the evaluation produced
no I/O (for security), then the command sents bytes produced from the
source to @racket[(current-output-port)]. Information about the
process is sent to @racket[(current-error-port)].

This all happens under a @tech{runtime configuration}, so transfers
can be halted by settings like @racket[DENXI_FETCH_TOTAL_SIZE_MB].
}


@section{CLI Functional Testing}

@defmodule[(submod denxi/cli test)]

Reprovides @racketmodname[racket],
@racketmodname[racket/runtime-path], and @tt{rackunit}.


@defproc[(functional-test/install-all [definition-variant
                                       racket-module-input-variant/c]) void?]{
Effect: For each output of the given @tech{package definition}, create
an ephemeral @tech{workspace}, install the output, and then uninstall
the output.

Call only in the context of a unit test. This function makes many
assertions.

If this function runs to completion, the current
@tech/reference{parameterization} is compatible with the given package
definition.
}

@defproc[(functional-test/install-one [definition-variant
                                       racket-module-input-variant/c]
                                      [output-name string? DEFAULT_NAME])
                                      void?]{
Like @racket[functional-test/install-all], but for a single output
in the definition.
}

@defproc[(check-cli [arguments (or/c (listof string?) (vectorof string?))]
                    [continue (-> exit-code/c
                                  program-log/c
                                  bytes?
                                  bytes?
                                  any)])
                                  any]{
Sends command line to Denxi to processing.

Applies @racket[continue] in tail position to the following arguments:

@itemlist[#:style 'ordered
@item{The exit code of the program}
@item{The @tech{program log} at the end of evaluation.}
@item{The contents of standard output after evaluation. Includes localized form of program log.}
@item{The contents of standard error after evaluation.}
]

@racket[continue] may therefore make assertions about the functional
correctness of a command line.
}

@defproc[(test-cli [message string?]
                   [arguments (or/c (listof string?) (vectorof string?))]
                   [continue (-> exit-code/c
                                 program-log/c
                                 bytes?
                                 bytes?
                                 any)])
                                 any]{
The test form of @racket[check-cli].
}

@defproc[(get-checked-links) (hash/c path-string? path-string?)]{
Run the @racket[show-command] with @litchar{links}, makes related
assertioins on the data, then returns a hash table.  The keys of the
hash tables are paths to links. The values are the targets of the
links.

Relative paths are made in regards to @racket[(DENXI_WORKSPACE)].
}

@defproc[(get-checked-installed-outputs) (listof (list/c exact-package-query? string? path-string?))]{
Run the @racket[show-command] with @litchar{installed}, makes related
assertions on the data, then returns a list of lists. The first
element of a sublist is an @tech{exact package query} that identifies
a package definition responsible for an output. The second element is
the name of that output. The third element, when passed to
@racket[build-object-path], returns the path to the installed output.
}


@defproc[(check-garbage-collection [ok? predicate/c]) void?]{
Run the @litchar{gc} command, assert expected behaviors, then assert
@racket[ok?] returns @racket[#t] when applied to the number of bytes
recovered.
}
