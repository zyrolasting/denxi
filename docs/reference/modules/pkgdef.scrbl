#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    xiden/logged
                    xiden/package
                    xiden/rc
                    @except-in[xiden/pkgdef do #%module-begin]]
         "../../shared.rkt"]

@title{Module Language}

@defmodule[xiden/pkgdef]

@racketmodname[xiden/pkgdef] is a module language superset
of @racketmodname[setup/infotab].


@section{Special Bindings}

@defproc[(install [link-path path-string?] [output-name string?] [pkgdef-source string?]) void?]{
Builds an output named @racket[output-name] in the @tech{workspace}, and
creates a new symbolic link to the output directory at @racket[link-path]
(w.r.t. @racket[(current-directory)]).

The output is defined in regards to the @tech{package definition} created using
@racket[(fetch pkgdef-source)].
}


@defproc[(run [#:expected-exit-codes expected-exit-codes (non-empty-listof exact-nonnegative-integer?) '(0)]
              [#:timeout timeout real? (XIDEN_SUBPROCESS_TIMEOUT_S)]
              [command path-string?]
              [arg string?] ...)
              void?]{
Runs a subprocess synchronously, and under the restrictions set by
@racket[XIDEN_ALLOW_ENV] and @racket[XIDEN_TRUSTED_EXECUTABLES].

Standard output and standard error are both redirected to
@racket[(current-output-port)]. Standard input is disabled.

@racket[run] raises an exception if

@itemlist[
@item{the subprocess finished, and the exit code is not one of the elements in @racket[expected-exit-codes].}
@item{the subprocess did not complete in @racket[(min timeout (XIDEN_SUBPROCESS_TIMEOUT_S))] seconds.}
@item{the @tech{runtime configuration} does not allow execution of the command.}
]
}

@racketblock[
(code:comment "Use xiden do ++bin ls ...")
(define (build target)
  (run "ls" "-l"))
]

