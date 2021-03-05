#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    xiden/message
                    xiden/integrity
                    xiden/openssl
                    xiden/rc]
         racket/format
         xiden/integrity
         @for-syntax[racket/base]
         "../../shared.rkt"]

@title{Integrity Checking}

@defmodule[xiden/integrity]


@defstruct*[integrity-info ([algorithm md-algorithm/c] [digest bytes?])]{
Represents integrity information for bytes. Given bytes from some
source, the bytes pass an @tech{integrity check} if they, when applied to
@racket[algorithm], produce a value @racket[equal?] to
@racket[digest].
}


@defthing[well-formed-integrity-info/c contract?]{
A contract that recognizes instances of @racket[integrity-info], and
verifies if the field values are logically consistent. It checks if
the hash algorithm is valid, and if the digest is a byte string. It does
not verify if the digest length is appropriate for the algorithm.
}


@defproc[(check-integrity [#:trust-bad-digest trust-bad-digest any/c] [intinfo (or/c #f integrity-info?)] [variant md-bytes-source/c]) $integrity?]{
Performs an @deftech{integrity check}. See @racket[$integrity].

If @racket[trust-bad-digest] is a true value, the integrity check
passes unconditionally. Otherwise, the check passes if a message
digest derived from @racket[variant] is consistent with
@racket[intinfo].

The check fails in all other conditions.
}


@defstruct*[($integrity $message) ([ok? boolean?] [stage symbol?] [info any/c]) #:prefab]{
A @tech{message} that reports the results of an integrity check.

@racket[ok?] is @racket[#t] if the check passed.

@racket[stage] is a symbol that tells a maintainer which procedure
created the given @racket[$integrity] instance.

@racket[info] is the Racket value presented as integrity information
for the check.
}


@defproc[(integrity [algorithm md-algorithm/c] [digest bytes?]) well-formed-integrity-info/c]{
An abbreviated constructor for @racket[integrity-info] that performs stronger validation on arguments.

Meant for use in @tech{package definitions} when declaring @tech{package inputs}.
}

@defproc[(bind-trust-list [trusted (listof well-formed-integrity-info/c)]) (-> path-string? boolean?)]{
Returns a procedure @racket[P], such that @racket[(P
"/path/to/file")] (for example) is @racket[#t] if the given file
passes an @tech{integrity check} for one of the
@racket[integrity-info] structures in @racket[trusted].
}
