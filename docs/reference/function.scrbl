#lang denxi/document

@title{Functions}

@defmodule[denxi/function]

@racketmodname[denxi/function] extends and reprovides
@racketmodname[racket/function].

@defform[(async . body)]{
Equivalent to @racket[(thread (thunk . body))]
}

@defproc[(keep-values [v any/c] ...) (-> any)]{
Equivalent to @racket[(thunk (values v ...))].
}
