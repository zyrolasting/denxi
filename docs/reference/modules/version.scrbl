#lang scribble/manual

@require[@for-label[racket/base racket/contract xiden/version]
          "../../shared.rkt"]

@title{Versions}

@defmodule[xiden/version]

A @tech{package definition} has a @tech{version}.

A @deftech{version} is a combination of an @tech{edition} and a
@tech{revision}.

An @deftech{edition} is a non-empty string.

An @tech{edition} has at least one @tech{revision}.

A @deftech{revision} is either a @tech{revision number} or a @tech{revision name}.

A @deftech{revision number} is an exact non-negative integer.

A @deftech{revision name} is a string alias for a @tech{revision number}.

A @tech{revision name} should not repeat within an @tech{edition}, but
may repeat across editions.

@tech{Revision numbers} start counting from @racket[0].


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


@defproc[(make-minimum-revision-number [boundary revision-number?]
                                       [#:exclusive? exclusive? any/c])
                                       revision-number?]{
Returns a revision number, interpreting another as a minimum boundary
for an integer interval. If @racket[exclusive?] is @racket[#f], then
the return value is simply @racket[boundary]. Otherwise, it's
@racket[(add1 boundary)].
}

@defproc[(make-maximum-revision-number [boundary revision-number?]
                                       [#:exclusive? exclusive? any/c])
                                       revision-number?]{
Returns a revision number, interpreting another as a maximum boundary
for an integer interval. If @racket[exclusive?] is @racket[#f], then
the return value is simply @racket[boundary]. Otherwise, it's
@racket[(max 0 (sub1 boundary))].
}

@defproc[(find-latest-available-revision-number [available? (-> revision-number? any/c)]
                                                [lo revision-number?]
                                                [hi revision-number?])
                                                (or/c #f revision-number?)]{
Finds the largest element of the inclusive interval @litchar|{{lo
.. hi}}| where @racket[available?] returns a true value. Returns
@racket[#f] if no such element exists.
}

@defproc[(find-oldest-available-revision-number [available? (-> revision-number? any/c)]
                                                [lo revision-number?]
                                                [hi revision-number?])
                                                (or/c #f revision-number?)]{
Finds the smallest element of the inclusive interval @litchar|{{lo
.. hi}}| where @racket[available?] returns a true value. Returns
@racket[#f] if no such element exists.
}
