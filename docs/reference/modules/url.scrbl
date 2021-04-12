#lang scribble/manual

@require[@for-label[racket/base racket/contract xiden/url]
          "../../shared.rkt"]

@title{URLs}

@defmodule[xiden/url]

@racketmodname[xiden/url] reprovides @racketmodname[net/url], along
with the following bindings.

@defproc[(url-variant? [s any/c]) boolean?]{
Returns @racket[(or (url? s) (url-string? s))].
}


@defproc[(url-string? [s any/c]) boolean?]{
Returns @racket[#t] if @racket[s] is a string that can be parsed as an
URL using @racket[string->url].
}

@defproc[(coerce-url [s url-variant?]) url?]{
Returns an instance of @racket[url] in terms of a variant type.
}

@defproc[(coerce-url-string [s url-variant?]) url-string?]{
Returns a @racket[url-string?] in terms of a variant type.
}

@defidform[#:kind "syntax class" url-string]{
A syntax class for @racket[url-string?] string literals.
}
