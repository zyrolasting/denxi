#lang denxi/document

@title{Mind}

@defmodule[denxi/mind]

@section{Mind Interface}


@defthing[gen:mind]{
A @deftech{mind} is an implementation of the @racket[gen:mind]
@tech/reference{generic interface}. A mind operates on its own
@tech{knowns}, in a manner similar to key-value storage.
}

@defthing[mind-implementation/c contract?]{
A contract for complete implementations of @racket[gen:mind].  The
contract for each method matches the method's entry in this section.
}


@defproc[(mind-knowns [m mind?]) (subprogram/c (sequence/c bytes? known?))]{
Returns a subprogram that computes all knowns available in the mind.
}

@defproc[(mind-recall [m mind?] [key bytes?] [learn (-> known? (subprogram/c void?))])
         (subprogram/c known?)]{
Returns a subprogram to access exactly one @tech{known}.

In the event the subprogram finds no record of a known using
@racket[key], it must eagerly create a compatible known @racket[k] and
run the @racket[(learn k)] subprogram to populate @racket[k].  Unless
@racket[(learn k)] fails, the mind must bind @racket[key] to
@racket[k] before providing @racket[k] as the subprogram value.
}

@defproc[(mind-forget [m mind?] [key bytes?]) (subprogram/c exact-nonnegative-integer?)]{
Returns a subprogram that removes a @tech{known} from storage, and
computes the number of bytes freed in doing so.
}


@section{Remaining Mind Bindings}

@defthing[mind-implementation/c contract?]{
A contract that recognizes a full implementation of @racket[gen:mind].
}

@defproc[(mind-clean [m mind?]) (subprogram/c exact-nonnegative-integer?)]{
Returns a @tech{subprogram} that removes all nameless @tech{knowns}
from the given mind, via @racket[mind-forget]. The computed value is
the total number of bytes recovered.
}
