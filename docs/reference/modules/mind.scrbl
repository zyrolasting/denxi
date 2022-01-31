#lang scribble/manual

@require["../../shared.rkt"
         @for-label[racket/base
                    racket/contract
                    racket/format
                    racket/path
                    racket/sequence
                    denxi/codec
                    denxi/integrity
                    denxi/message
                    denxi/mind
                    denxi/string
                    denxi/port
                    denxi/version]
        denxi/mind
        @for-syntax[denxi/mind]]

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

@defproc[(mind-recall [m mind?] [key bytes?]) (subprogram/c known?)]{
Returns a subprogram that returns exactly one @tech{known}.  The
subprogram can only fail when no option to return a functional known
exists.
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