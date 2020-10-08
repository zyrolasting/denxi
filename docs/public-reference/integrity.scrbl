#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    xiden/message
                    xiden/integrity
                    xiden/rc]
         racket/format
         racket/pretty
         xiden/integrity
         @for-syntax[racket/base]
         "../shared.rkt"]

@title{Integrity Checking}

@defmodule[xiden/integrity]


@defstruct*[integrity-info ([algorithm md-algorithm/c] [digest bytes?])]{
Represents integrity information for bytes. Given bytes from some
source, the bytes pass an integrity check if they, when applied to
@racket[algorithm], produce a value @racket[equal?] to
@racket[digest].
}


@defthing[md-algorithm/c flat-contract? #:value (apply or/c md-algorithms)]{
A contract that accepts one of the symbols in @racket[md-algorithms].
}


@defthing[md-algorithms (listof symbol?)]{
A list of symbols that represent supported message digest algorithms.

Bound to @typeset-code[(pretty-format #:mode 'print md-algorithms)]
}


@defthing[md-bytes-source/c flat-contract? #:value (or/c path-string? bytes? input-port?)]{
This contract matches a value @racket[V] suitable for use in @racket[make-digest].

Given @racket[(path-string? V)], the bytes are drawn from the file located at @racket[V].
Given @racket[(bytes? V)] or @racket[(input-port? V)], the bytes are drawn directly from @racket[V].
}


@defthing[well-formed-integrity-info/c contract?]{
A contract that recognizes instances of @racket[integrity-info], and
verifies if the field values are logically consistent. It checks if
the hash algorithm is valid, and if the digest has the correct length
for that algorithm.
}


@defproc[(check-integrity [#:trust-bad-digest trust-bad-digest any/c] [intinfo any/c] [variant md-bytes-source/c]) $integrity?]{
Returns an instance of @racket[$integrity] @racket[I], such that
@racket[($integrity-status I)] reports the relationship between the
user's configuration, the bytes from @racket[variant], and the
integrity information in @racket[intinfo].

@racket[intinfo] must pass @racket[well-formed-integrity-info/c] to be
used in a full integrity check. Otherwise @racket[($integrity-status
I)] will be bound to @racket['missing].

If @racket[trust-bad-digest] is a true value, the integrity check is
skipped. In that case, @racket[($integrity-status I)] will be bound to
@racket['trusted].
}


@defstruct*[($integrity $message) ([algorithm md-algorithm/c] [status (or/c 'mismatch 'verified 'trusted 'mismatch)]) #:prefab]{
A @tech{message} pertaining to the integrity of a @tech{package input}'s
bytes from a source.  The meaning of the message depends on the value
bound to the @racket[status] field:

@itemlist[
@item{@racket['trusted]: The bytes were not checked. This only happens when @racket[(XIDEN_TRUST_BAD_DIGEST)] is
@racket[#t].}

@item{@racket['verified]: The hash @racket[algorithm] applied to the bytes produced the expected digest.}

@item{@racket['missing]: No well formed integrity information was declared, making it impossible to compare digests.}

@item{@racket['mismatch]: The hash @racket[algorithm] applied to the bytes produced a different digest.}
]

}


@defproc[(integrity [algorithm md-algorithm/c] [digest bytes?]) well-formed-integrity-info/c]{
An abbreviated constructor for @racket[integrity-info] that performs stronger validation on arguments.

Meant for use in @tech{package definitions} when declaring @tech{package inputs}.
}

@defproc[(make-digest [variant md-bytes-source/c] [algorithm md-algorithm/c]) bytes?]{
Returns the raw byte content of @racket[algorithm] applied to bytes from @racket[variant].
}

@defproc[(passed-integrity-check? [I $integrity?]) boolean?]{
Returns @racket[#t] if @racket[($integrity-status I)] is
@racket['trusted] or @racket['verified].
}
