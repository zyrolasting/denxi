#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    xiden/logged
                    xiden/package
                    xiden/rc]
         "../../shared.rkt"]

@title{Package API}

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


@section{Package Messages}

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
