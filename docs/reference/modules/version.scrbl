#lang scribble/manual

@require[@for-label[racket/base racket/contract xiden/version]
          "../../shared.rkt"]

@title{Package Definition Versions}

A @tech{package definition} may contain a @tech{version}.

A @deftech{version} is a combination of an @tech{edition} and a
@tech{revision}.

@section{Editions}

An @deftech{edition} is a non-empty string.

A @tech{package definition}'s @tech{edition} is bound to the @racket[edition] identifier.

The default name for an edition is @racket{default}.

An @tech{edition} has at least one @tech{revision}.


@section{Revisions}

A @deftech{revision} is either a @tech{revision number} or a @tech{revision name}.

A @deftech{revision number} is an exact non-negative integer.

A @deftech{revision name} is a string alias for a @tech{revision number}.

A @tech{package definition} must bind a @tech{revision number} to
@racketid[revision-number], and (optionally) at least zero
@tech{revision names} to @racketid[revision-names].

A @tech{revision name} should not repeat within an @tech{edition}, but
may repeat across editions.

@tech{Revision numbers} start counting from @racket[0].


@section{Package Version API}

@defmodule[xiden/version]

@defthing[revision-number? predicate/c]{
Returns @racket[#t] if the input value is useable as a @tech{revision number}.
}

@defthing[revision-number-string? predicate/c]{
Returns @racket[#t] if the input value is a string that, when converted to a
number, is useable as a @tech{revision number}.
}

@defproc[(make-revision-interval [lo revision-number?]
                                 [hi revision-number?]
                                 [#:lo-exclusive lo-exclusive any/c]
                                 [#:hi-exclusive hi-exclusive any/c])
                                 (values revision-number? revision-number?)]{
Creates a (possibly invalid) integer interval using

@racketblock[
(values (if lo-exclusive (add1 lo) lo)
        (if hi-exclusive (sub1 hi) hi))]
}
