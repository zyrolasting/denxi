#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    xiden/message
                    xiden/integrity
                    xiden/openssl
                    xiden/source]
         racket/format
         xiden/integrity
         @for-syntax[racket/base
                     xiden/integrity]
         "../../shared.rkt"]

@title{Integrity Checking}

@defmodule[xiden/integrity]


@defstruct*[integrity-info ([algorithm chf/c] [digest source-variant?])]{
Represents integrity information for bytes. Given bytes from some
source, the bytes pass an @tech{integrity check} if they, when applied to
@racket[algorithm], produce a value @racket[equal?] to
@racket[digest].
}

@defproc[(make-trusted-integrity-info [source source-variant?] [chf chf/c DEFAULT_CHF]) integrity-info?]{
Returns integrity information with a new digest computed from the
@racket[source]'s bytes. No safety limits are places on bytes drawn
from the source, so this procedure should only be used on trusted
data.
}


@defthing[well-formed-integrity-info/c contract?]{
A contract that recognizes instances of @racket[integrity-info], and
verifies if the field values are logically consistent. It checks if
the hash algorithm is valid, and if the digest is a byte string. It does
not verify if the digest length is appropriate for the algorithm.
}


@defproc[(fetch-digest [intinfo well-formed-integrity-info/c] [exhaust exhaust/c]) any/c]{
Like @racket[fetch], except the raw bytes of a message digest are
sourced from @racket[(integrity-info-digest intinfo)] and returned as
a @racket[bytes?] value. If the source is exhausted, then this returns
the value from @racket[exhaust].
}

@defproc[(check-integrity [#:trust-bad-digest trust-bad-digest any/c] [intinfo (or/c #f integrity-info?)] [variant source-variant?]) $integrity?]{
Performs an @deftech{integrity check}. See @racket[$integrity].

If @racket[trust-bad-digest] is a true value, the integrity check
passes unconditionally. Otherwise, the check passes if a message
digest derived from @racket[variant] is consistent with
@racket[intinfo].

The check fails in all other conditions.
}


@defsetting*[XIDEN_TRUST_BAD_DIGEST]{
@bold{Highly dangerous}. When true, disable integrity checking.
}

@defstruct*[($integrity $message) ([ok? boolean?] [stage symbol?] [info any/c]) #:prefab]{
A @tech{message} that reports the results of an integrity check.

@racket[ok?] is @racket[#t] if the check passed.

@racket[stage] is a symbol that tells a maintainer which procedure
created the given @racket[$integrity] instance.

@racket[info] is the Racket value presented as integrity information
for the check.
}


@defproc[(integrity [algorithm chf/c] [digest bytes?]) well-formed-integrity-info/c]{
An abbreviated constructor for @racket[integrity-info] that performs stronger validation on arguments.

Meant for use in @tech{package definitions} when declaring @tech{package inputs}.
}

@defproc[(bind-trust-list [trusted (listof well-formed-integrity-info/c)]) (-> path-string? boolean?)]{
Returns a procedure @racket[P], such that @racket[(P
"/path/to/file")] (for example) is @racket[#t] if the given file
passes an @tech{integrity check} for one of the
@racket[integrity-info] structures in @racket[trusted].
}

@defproc[(make-sourced-digest [variant source-variant]
                              [algorithm chf/c]
                              [exhaust exhaust/c raise]) bytes?]{
Like @racket[make-digest], except the digest is produced using bytes
@tech{tapped} from @racket[variant] using @racket[fetch].
}

@defproc[(lock-integrity-info [to-lock well-formed-integrity-info/c]
                              [exhaust exhaust/c])
                              well-formed-integrity-info/c]{
Returns new integrity information, such that digest bytes are within
the instance itself if @racket[exhaust] is not called.
}

@defthing[MAX_EXPECTED_DIGEST_LENGTH budget/c]{
A maximum expected number of bytes occupied by some digest.  Chosen
empirically.
}
