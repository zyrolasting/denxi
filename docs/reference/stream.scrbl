#lang denxi/document

@title{Streams}

@defmodule[denxi/stream]

@racketmodname[denxi/stream] extends and reprovides
@racketmodname[racket/stream].

@defproc[(stream-exactly-one [s stream?] [fail-thunk (-> any)]) any]{
Returns

@itemlist[
@item{@racket[(fail-thunk)], if @racket[s] has zero elements, or more than one element.}
@item{@racket[(stream-first s)], if @racket[s] has exactly one element.}
]
}

@defproc[(stream-next [s stream?] [fail-thunk (-> any)]) any]{
Returns (fail-thunk)], if @racket[s] is empty. Returns
@racket[(stream-first s)] otherwise.
]
}

@defproc[(stream-consume [s stream?] [consequent (-> any/c stream? any)] [alternate (-> any)]) any]{
Returns @racket[(consequent (stream-first s) (stream-rest s))], or
@racket[(alternate)] if the stream is empty.
]
}
