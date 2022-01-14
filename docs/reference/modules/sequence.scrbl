#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    denxi/sequence]
         "../../shared.rkt"]

@title{Sequences}

@defmodule[denxi/sequence]

@racketmodname[denxi/sequence] extends and reprovides
@racketmodname[racket/sequence].


@defproc[(in-cartesian-product [gears (sequence/c sequence?)]) (sequence/c vector?)]{
Returns a @tech/reference{sequence} of all vectors from @racket[(odometer gears)].
}

@defproc[(in-cartesian-map [f procedure?] [gears (sequence/c sequence?)]) sequence?)]{
Like @racket[in-cartesian-product], but each vector @racket[V] is replaced by

@racketblock[
(call-with-values (curry vector->values V) f)
]
}

@defproc[(odometer [gears (sequence/c sequence?)])
         (values exact-nonnegative-integer?
                 (-> exact-nonnegative-integer? vector?))]{
Returns two values, @racket[max-ordinal] and @racket[ordinal->vector].

@racket[ordinal->vector] models an abstract odometer. Use to implement
a cartesian product using lazy evaluation.

@racket[(ordinal->vector 0)] returns a vector representing the initial
state of the odometer.

@racket[(ordinal->vector max-ordinal)] returns a vector representing
the final state of the odometer.

In general, @racket[(ordinal->vector ordinal)] returns a vector
containing one element selected from each subsequence of
@racket[gears]. If @racket[(> ordinal max-ordinal)], then
@racket[ordinal] is implicitly treated as @racket[max-ordinal].
}
