#lang denxi/document

@title{Formatting}

@defmodule[denxi/format]

@racketmodname[denxi/format] provides all bindings from
@racketmodname[racket/format], including the below bindings.

@section{Conventional Formatting Procedures}

@defproc[(format-values [make-values (-> any)]) string?]{
Returns a string that starts with @litchar{[} and ends with
@litchar{]}. Using @racket[read] on this string will produce a
Racket list.

The string encodes at least zero values returned from
@racket[make-values] as elements of the list. Each element is
formatted using @racket[format-value].
}


@defproc[(format-value [value any/c]) string?]{
Returns a string selected to represent @racket[value].

Selection criteria apply in order.

@racket[string?]: Return @racket[value]

@racket[symbol?]: Return @racket[(format "~v" value)]

@racket[stream?]: Return a string encoding of a readable Racket list.
All data found using @racket[stream-first] are encoded using
@racket[format-values].

@racketblock[
(equal? "(['a 1])" (in-hash (hash 'a 1))]
]

@racket[srcloc?]: Return @racket[(srcloc->string value)]

@racket[number?]: Return @racket[(format "~s" value)]

@racket[sequence?]: Return @racket[(format-value (sequence->stream value))]

@racket[pair?]: Return recursively formatted values using @racket{(~a . ~a)}

@racket[continuation-mark-set?]: Return @racket[(format-value (continuation-mark-set->context v))]

@racket[struct?]: Return @racket[(format-value (in-vector (struct->vector v)))]

In all other cases, the return value is either @racket[(~s value)] or
@racket[(~s (~s value))], depending on which is suitable for use with
@racket[read]. The latter case captures non-readable forms,
e.g. @racket[(format-value void)].
}

@defproc[(indent-lines [lines (listof string?)]) (listof string?)]{
Returns a new version of @racket[lines] where each element has two
leading spaces.
}

@defproc[(join-lines [#:trailing? trailing? any/c #f]
                     [#:suffix suffix (or/c char? string? #f)]
                     [lines (listof string?)]) string?]{
Combines @racket[lines] into a single string.

If @racket[suffix] is @racket[#f], then @racket[join-lines] will use a
platform-specific suffix.

If @racket[trailing?] is @racket[#t], then the last line will also end
in the suffix.
}
