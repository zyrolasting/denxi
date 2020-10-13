#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    xiden/logged
                    xiden/package
                    xiden/rc
                    @except-in[xiden/pkgdef #%module-begin]]
         "../shared.rkt"]

@title{Packages}

This section covers the languages used to express @tech{package
definitions}, and the @tech{packages} created from them.

A @deftech{package} is an active instance of a @tech{package
definition}.  The difference between a @tech{package} and a
@tech{package definition} is therefore like the difference between a
process and a program, or a Docker container and a Docker image.

A @deftech{package definition} is a Racket module that combines
discovery information with a program. That program is used to create
and execute @tech{packages} to produce output files. A user declares
the inputs, outputs, and processing steps for that program with the
bindings described in this document.


@section{Reader Extension}

@(defmodulelang* (xiden))

@racketmodname[xiden], as a reader extension, defines a
@racketmodname[xiden/pkgdef] module. Any well-formed
@litchar|{#lang info}| document is a well-formed @litchar|{#lang
xiden}| document.

@tt{raco setup} and @tt{raco pkg} will not read @litchar|{#lang
xiden}| documents.


@section{Module Language}

@defmodule[xiden/pkgdef]

@racketmodname[xiden/pkgdef] is a module language superset
of @racketmodname[setup/infotab].


@subsection{Special Bindings}

@defproc[(install [link-path path-string?] [output-name string?] [pkgdef-source string?]) void?]{
Builds an output named @racket[output-name] in the @tech{workspace}, and
creates a new symbolic link to the output directory at @racket[link-path]
(w.r.t. @racket[(current-directory)]).

The output is defined in regards to the @tech{package definition} created using
@racket[(fetch pkgdef-source)].
}


@subsection{Additional Bindings}

@itemlist[
@item{@racket[λ]}
@item{@racket[#%app]}
@item{@racket[#%top-interaction]}
@item{@racket[base32]}
@item{@racket[base64]}
@item{@racket[build-path]}
@item{@racket[build-workspace-path]}
@item{@racket[call-with-input]}
@item{@racket[car]}
@item{@racket[case]}
@item{@racket[cdr]}
@item{@racket[current-directory]}
@item{@racket[define-values]}
@item{@racket[error]}
@item{@racket[from-catalogs]}
@item{@racket[from-file]}
@item{@racket[hex]}
@item{@racket[input]}
@item{@racket[input-ref]}
@item{@racket[integrity]}
@item{@racket[keep-input!]}
@item{@racket[lambda]}
@item{@racket[let]}
@item{@racket[let-values]}
@item{@racket[signature]}
@item{@racket[sources]}
@item{@racket[unpack]}
@item{@racket[void]}
]


@section{Package API}

@defmodule[xiden/package]

@defthing[DEFAULT_OUTPUT string? #:value "default"]{
The name of an output implicitly defined in all @tech{package definitions}.
}

@defproc[(run-package [package-definition-variant any/c]
                      [#:output-name output string? DEFAULT_OUTPUT]
                      [#:link-path link-path (or/c #f path-string?) #f])
                      logged?]{
Returns a @tech{logged procedure} called for its effect.  The effect
being that a symbolic link gets created at @racket[link-path],
pointing to a directory. That directory contains the files
corresponding to the @racket[output] defined in
@racket[pkgdef-variant]).

The @tech{logged procedure} is not atomic, so failure may result in
a broken intermediate state on disk. This procedure should be used
in the context of a transaction.

All @racket[run-package] @tech{messages} are instances of
@racket[$package].
}


@subsection{Package Messages}

@defstruct*[($package $message) () #:prefab]{
A @tech{message} from a package's runtime.
}

@defstruct*[($package:log $package) ([query xiden-query-string?]
                                     [output-name string?]
                                     [messages messy-log/c]) #:prefab]{
A @tech{message} containing other messages relevant to building a particular package output.
}

@defstruct*[($package:output $package) () #:prefab]{
A @tech{message} pertaining to a package output.
}

@defstruct*[($package:output:built $package:output) () #:prefab]{
@project-name successfully built a package output.
}

@defstruct*[($package:output:reused $package:output) () #:prefab]{
@project-name reused a previously-built package output.
}

@defstruct*[($package:output:undefined $package:output) () #:prefab]{
A requested output is not defined in a corresponding @tech{package
definition}.
}

@defstruct*[($package:definition $package) () #:prefab]{
A @tech{message} pertaining to a @tech{package definition}.
}

@defstruct*[($package:definition:undeclared-racket-version $package:definition) () #:prefab]{
A @tech{package definition} did not define supported Racket versions.
}

@defstruct*[($package:definition:unsupported-racket-version $package:definition)
            ([versions racket-version-ranges/c]) #:prefab]{
A @tech{package definition} does not claim to support the running version of Racket.
}

@defstruct*[($package:definition:value $package:definition)
            ([id symbol?]) #:prefab]{
A @tech{message} pertaining to a specific value in a @tech{package definition}.
The value may or may not be bound, but if it is bound, it is expected to be bound
to @racket[id] in the @tech{package definition}'s namespace.
}

@defstruct*[($package:definition:value:missing $package:definition:value)
            () #:prefab]{
A value in a @tech{package definition} is required, but not bound.
}

@defstruct*[($package:definition:value:invalid $package:definition:value)
            ([value any/c]) #:prefab]{
A value in a @tech{package definition} is bound, but it violates a contract.
}
