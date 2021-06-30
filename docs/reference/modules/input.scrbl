#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/path
                    xiden/artifact
                    xiden/dig
                    xiden/subprogram
                    xiden/message
                    xiden/integrity
                    xiden/input
                    xiden/monad
                    xiden/signature
                    xiden/source
                    xiden/string]
         "../../shared.rkt"]

@title{Package Inputs}

@defmodule[xiden/input]

@defstruct*[package-input ([name string?] [plinth (or/c any/c artifact?)])]{
A @deftech{package input} is an instance of @racket[package-input].
Each instance represents data that a package may lazily fetch.

The @racket[plinth] acts as a placeholder for a value used to retrieve
an @tech{artifact}.  If the plinth does not hold an instance of
@racket[artifact], then we use that value in the @tech{digsite
metaphor} to find an artifact.
}

@defproc[(make-package-input [name string?]
                             [plinth (or/c any/c artifact?)
                                     #f])
                             package-input?]{
A contracted constructor for @racket[package-input].
}

@defthing[abstract-package-input? predicate/c]{
Returns @racket[#t] if the argument is an @deftech{abstract package
input}, meaning that @racket[package-input-plinth] is @racket[#f].
}

@defthing[current-inputs (parameter/c (listof package-input?))]{
A @tech/reference{parameter} bound to inputs to use with @racket[input-ref].
}

@defproc[(input-ref [name string?]) (subprogram/c package-input?)]{
Returns the first element of @racket[(current-inputs)] with an
@racket[package-input-name] @racket[equal?] to @racket[name].
}

@defproc[(find-input [inputs (listof package-input?)] [name string?]) (subprogram/c package-input?)]{
Returns a @tech{subprogram} @racketid[P] that either returns the
first input in @racket[inputs] with the given name, or fails with
@racket[$input:not-found] on the program log.
}

@defproc[(release-input [input package-input?]) (subprogram/c void?)]{
Returns a @tech{subprogram} @racketid[P] that deletes the
symbolic link derived from @racket[input].
}

@defproc[(resolve-input [input package-input?]) (subprogram/c path-string?)]{
Returns a @tech{subprogram} @racketid[P] that, when applied,
acquires and verifies the bytes for @racket[input]. @racketid[P]
returns a relative path to a symbolic link in
@racket[(current-directory)].

@racketid[P] may fail for many possible reasons, which will appear in
the program log.
}

@defproc[(keep-input [name string?]) (subprogram/c path-string?)]{
Equivalent to @racket[(mdo i := (input-ref name) (resolve-input i))].

Use for inputs that you do not intend to release.
}

@defstruct*[($input $message) ([name string?])]{
A @tech{message} regarding an input with a given name.
}

@defstruct*[($input:not-found $input) ()]{
Represents a failure to find an input using @racket[find-input].
}

@defstruct[untrusted-source ([input package-input?])]{
A @tech{source} that, when @tech{tapped}, yields bytes found using the
input. If no bytes are found or do not pass security checks, the
source is @tech{exhausted}. @racket[$transfer] messages use the
input's name.

This is the only @tech{source} that includes all possible safety
checks and customizations defined by the @tech{launcher}, making it
suitable for use with arbitrary data.
}

@defproc[(find-artifact-for-input [in package-input?]) (subprogram/c artifact?)]{
Returns a @tech{subprogram} that attempts to create an @tech{artifact}
from @racket[in].
}
