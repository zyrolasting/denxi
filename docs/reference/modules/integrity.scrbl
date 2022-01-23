#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    denxi/message
                    denxi/integrity
                    denxi/crypto
                    denxi/source]
         racket/format
         denxi/integrity
         @for-syntax[racket/base
                     denxi/integrity]
         "../../shared.rkt"]

@title{Integrity Checking}

@defmodule[denxi/integrity]

@racketmodname[denxi/integrity] reprovides
@racketmodname[denxi/integrity/base], and is meant for use as a
high-level interface to the integrity checking subsystem.

@section{High-level Integrity Operations}

@defproc[(load-builtin-chf [name symbol?]
                           [fail-thunk (-> any) (lambda () (raise ($chf-unavailable name)))])
                           any]{
Returns a built-in @racket[chf-impl/c] procedure, or
@racket[(fail-thunk)] if no implementation is available.

The output of this function may change across systems.
}


@defproc[(lock-integrity [#:digest-budget digest-budget budget/c MAX_EXPECTED_DIGEST_LENGTH]
                         [to-lock well-formed-integrity?]
                         [exhaust exhaust/c raise])
                         well-formed-integrity?]{
Returns a new @racket[integrity] instance @racketid[L].

Or, if the integrity information uses a @tech{source} for the digest
and that source is exhausted, returns @racket[exhaust] applied to the
contextual failure value.

@racket[(raw-integrity? L)] is @racket[#t] if a new digest can be
created using no more than @racket[digest-budget] bytes of overhead in
the content. Otherwise, @racket[L] is @racket[eq?] to @racket[to-lock].
}


@defproc[(make-trusted-integrity [source source-variant?]
                                 [chf symbol? (get-default-chf)])
                                 raw-integrity?]{
Returns integrity information based on @racket[source]'s bytes. No
safety limits are places on bytes drawn from the source, so this
procedure should only be used on trusted data.
}


@deftogether[(
@defthing[sourced-integrity? flat-contract? #:value (struct/c integrity symbol? source?)]
@defthing[well-formed-integrity? flat-contract? #:value (or/c raw-integrity? sourced-integrity?)]
@defthing[malformed-integrity? flat-contract? #:value (not/c well-formed-integrity?)]
)]{
Duck typing contracts for @racket[integrity] instances.
}


@section{Integrity Settings}

@defsetting*[DENXI_TRUST_BAD_DIGEST]{
@bold{Highly dangerous}. When true, disable integrity checking.
}

@defsetting*[DENXI_TRUST_CHFS]{
A list of possible canonical names for @racket[chf]s to trust when
checking data integrity.
}


@section{Integrity-based Trust}

@defproc[(bind-trust-list [trusted (listof integrity?)]
                          [chfs (listof chf?) (current-chfs)])
                          (-> input-port? boolean?)]{
Returns a procedure @racket[P].

@racket[(P input-port)] is @racket[#t] if it can reproduce an element
of @racket[trusted] using @racket[chfs].
}


@margin-note{@racket[make-user-chf-trust-predicate] is the fastest way
to check if a user trusts a CHF.}
@defproc[(make-user-chf-trust-predicate) (-> symbol? boolean?)]{
Returns @racket[(chf-bind-trust configured)], where
@racket[configured] combines existing values in
@racket[(current-chfs)] with the built-in implementations found in
@racket[(DENXI_TRUST_CHFS)].
}

@defproc[(build-builtin-chf-trust [trust-chfs (listof symbol?)]) (listof chf?)]{
Returns a value suitable for use in @racket[current-chfs].

The implementations of the given CHFs come from a bundled C library,
any input symbols must appear in @racket[(integrity-ffi-get-c-chfs!)].
}

@section{Prototyping Integrity Checks}

@defthing[snake-oil-chf chf? #:value (chf 'sha1 #px"^(?i:sha-?1)$" sha1-bytes)]{
A CHF meant for use in tests, low-security contexts, or when
compatibility is more important than security.

Do not use in production if you can avoid doing so.
}

@defproc[(call-with-snake-oil-chf-trust [thunk (-> any)]) any]{
Call @racket[thunk] in tail position. While control is in the thunk,
@racket[(current-chfs)] is @racket[(list snake-oil-chf)].
}

@include-section{integrity/base.scrbl}
@include-section{integrity/ffi.scrbl}
