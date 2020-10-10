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
Performs an @deftech{integrity check} and returns an instance of
@racket[$integrity] @racket[I].

If @racket[trust-bad-digest] is a true value, the integrity check
passes unconditionally. Otherwise, the check passes if a message
digest derived from @racket[variant] is consistent with
@racket[intinfo].

The check fails in all other conditions.
}


@defstruct*[($integrity $message) ([ok? boolean?] [stage symbol?] [intinfo any/c]) #:prefab]{
A @tech{message} that reports the results of an integrity check, and
the return value of an @tech{affirmation procedure} defined by
@racketmod[xiden/integrity].

Given an instance @racket[I], @racket[(integrity-ok? I)] is
@racket[#t] if the check passed.

@racket[(integrity-stage I)] is @racket[eq?] to the
@racket[object-name] of the @tech{affirmation} used to conclude the
check. Note for auditing purposes that a stage other than
@racket[(object-name consider-digest-match)] indicates a higher trust
(and thus less secure) @tech{runtime configuration}.

@racket[($integrity-info I)] is the value presented as integrity
information for the check. It can be any Racket value.
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
