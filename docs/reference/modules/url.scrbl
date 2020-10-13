#lang scribble/manual

@require[@for-label[racket/base racket/contract xiden/url]
          "../../shared.rkt"]

@title{URLs}

@defmodule[xiden/url]

@racketmodname[xiden/url] reprovides @racketmodname[net/url], along
with the following bindings.

@defproc[(url-string? [s any/c]) boolean?]{
Returns @racket[#t] if @racket[s] is a string that can be parsed as an
URL using @racket[string->url].
}
