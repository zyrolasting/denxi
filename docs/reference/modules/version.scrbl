#lang scribble/manual

@require[@for-label[racket/base racket/contract xiden/version]
          "../../shared.rkt"]

@title{Package Definition Versions}

A @tech{package definition} has a @tech{version}.

A @deftech{version} is a combination of an @tech{edition} and a
@tech{revision}.

@section{Editions}

An @deftech{edition} is a non-empty string.

The default name for an edition is @racket{default}.

An @tech{edition} has at least one @tech{revision}.


@section{Revisions}

A @deftech{revision} is either a @tech{revision number} or a @tech{revision name}.

A @deftech{revision number} is an exact non-negative integer.

A @deftech{revision name} is a string alias for a @tech{revision number}.

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

@defproc[(revision-number-variant? [v any/c]) boolean?]{
Equivalent to @racket[(or/c revision-number? revision-number-string?)].
}

@defproc[(coerce-revision-number [v revision-number-variant?]) revision-number?]{
Returns a revision number in terms of a variant type.
}

@defproc[(coerce-revision-number-string [v revision-number-variant?]) revision-number-string?]{
Returns a revision number string in terms of a variant type.
}

@defproc[(make-revision-interval [lo (or/c revision-number? #f)]
                                 [hi (or/c revision-number? #f)]
                                 [#:lo-exclusive lo-exclusive any/c]
                                 [#:hi-exclusive hi-exclusive any/c])
                                 (values (or/c #f revision-number?)
                                         (or/c #f revision-number?))]{
Creates a possibly invalid integer interval using

@racketblock[
(values (if (and lo lo-exclusive) (add1 lo) lo)
        (if (and hi hi-exclusive) (sub1 hi) hi))]

@racket[lo] and @racket[hi] may be @racket[#f] to capture cases where
@tech{revision names} cannot map to @tech{revision numbers}. In that
case, the corresponding interval bound will also be @racket[#f].
}
