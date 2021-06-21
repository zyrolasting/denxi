#lang scribble/manual

@require["../../shared.rkt"
         @for-label[racket/base
                    racket/contract
                    xiden/message
                    xiden/integrity
                    xiden/signature
                    xiden/source]
         @for-syntax[xiden/signature]
         xiden/signature]

@title{Signature Checking}

@defmodule[xiden/signature]

@racketmodname[xiden/signature] extends @racket[xiden/crypto] with
utilities tailored for data authentication.


@defstruct*[signature-info ([pubkey source-variant?] [body source-variant?])]{
Holds an expression of a public key file and the bytes of a signature
created using a private key.
}

@defproc[(signature [pubkey source-variant?] [body source-variant?]) signature-info?]{
An abbreviated @racket[signature-info] constructor.
}

@defthing[well-formed-signature-info/c flat-contract?]{
Recognizes an instance of @racket[signature-info] that is suitable for
use with @racket[check-signature].
}

@defproc[(fetch-signature-payload [src source-variant?] [exhaust exhaust/c]) any/c]{
Like @racket[fetch], except transfer limits are capped to
@racket[MAX_EXPECTED_SIGNATURE_PAYLOAD_LENGTH] and no transfer status
information is reported. When the source has been successfully
@tech{tapped}, the return value is a byte string representing the full
content of the requested resource.

In practice, the fetched bytes are expected to contain either a public
key or a signature.  In any case, the output is assumed to be
compatible with the tool used to verify signatures.
}


@defthing[current-verify-signature
          (parameter/c (-> integrity-info?
                           signature-info?
                           boolean?))]{
A parameter containing a procedure for signature verification.
Returns @racket[#t] if Xiden may assume that the signature in the
second argument is valid for the digest found in the first
argument. Making this procedure always return @racket[#t] is
equivalent to setting @racket[XIDEN_TRUST_UNSIGNED] to @racket[#t].

The default implementation is a procedure that uses the host's OpenSSL
installation to verify the signature.

You may assume that the public key is trusted if control reaches this
procedure in the context of a @tech{launcher}.
}


@defproc[(check-signature [#:trust-public-key? trust-public-key? (-> input-port? any/c)]
                          [#:public-key public-key bytes?]
                          [#:trust-unsigned trust-unsigned any/c]
                          [#:trust-bad-digest trust-bad-digest any/c]
                          [siginfo (or/c #f signature-info?)]
                          [intinfo well-formed-integrity-info/c])
                          $signature?]{
This procedure returns the result of a @deftech{signature check},
which follows the high-level rules shown below. Each rule is processed
in the order shown.

If @racket[trust-bad-digest] is true, then the check passes regardless
of the value set for any other argument. This is because if any data
are trusted, then the same applies to any signature.

If @racket[trust-unsigned] is true and
@racket[(well-formed-signature-info/c siginfo)] is @racket[#f], then
the check passes. This is because a failure to declare a signature is
the same as not providing a signature. Trusting unsigned input means
being okay with this.

Given @racket[(trust-public-key? (open-input-bytes public-key))], both
@racket[siginfo] and @racket[intinfo] are well-formed, and the public
key verifies the signature against the integrity information, the
check passes. The check fails in all other conditions.
}


@defproc[(lock-signature-info [#:public-key-budget
                               public-key-budget
                               budget/c
                               MAX_EXPECTED_SIGNATURE_PAYLOAD_LENGTH]
                              [#:signature-budget
                               signature-budget
                               budget/c
                               MAX_EXPECTED_SIGNATURE_PAYLOAD_LENGTH]
                              [siginfo well-formed-signature-info/c]
                              [exhaust exhaust/c])
                              well-formed-signature-info/c]{
Like @racket[lock-integrity-info], but for the fields of a
@racket[signature-info] instance.
}

@defproc[(make-snake-oil-signature-info [digest bytes?]
                                        [chf chf/c DEFAULT_CHF])
                                        well-formed-signature-info/c]{
Return a new @racket[signature-info] using
@racket[snake-oil-public-key] and @racket[sign-with-snake-oil].  Do
not use in production code.
}


@defthing[MAX_EXPECTED_SIGNATURE_PAYLOAD_LENGTH budget/c]{
An estimated maximum number of bytes (chosen empirically) for a public
key or signature.
}

@defproc[(call-with-trust-in-snake-oil [f (-> any)]) any]{
Returns @racket[(f)]. While control is in @racket[f],
@racket[XIDEN_TRUST_CHFS] and @racket[XIDEN_TRUST_PUBLIC_KEYS] are
extended to trust @racket[DEFAULT_CHF] and
@racket[snake-oil-public-key], respectively.

Use to prototype new verifications in the context of an existing
configuration.
}

@defproc[(call-with-faith-in-snake-oil [f (-> any)]) any]{
Like @racket[call-with-trust-in-snake-oil], except
@racket[XIDEN_TRUST_CHFS] and @racket[XIDEN_TRUST_PUBLIC_KEYS] are
@italic{replaced} such that they @italic{only} trust
@racket[DEFAULT_CHF] and @racket[snake-oil-public-key], respectively.

Use to prototype verifications in what would otherwise be a zero-trust
configuration.
}

@defstruct*[($signature $message) ([ok? boolean?]
                                   [stage symbol?]
                                   [public-key (or/c #f bytes?)])
                                  #:prefab]{
A @tech{message} that reports the results of a signature check.

@racket[ok?] is @racket[#t] if the check passed.

@racket[stage] is a symbol representing the procedure responsible for
the check result.

@racket[public-key] is the public key used for a check, or @racket[#f]
if a public key is not available or relevant.
}

@defsetting*[XIDEN_TRUST_ANY_PUBLIC_KEY]{
@bold{Dangerous}. When true, trust any public key used to verify a signature.
}

@defsetting*[XIDEN_TRUST_UNSIGNED]{
@bold{Dangerous}. When true, trust any input that lacks a signature.
}

@defsetting*[XIDEN_TRUST_BAD_SIGNATURE]{
@bold{Dangerous}. When true, trust any input that has a signature that does not match the input's integrity information.
}

@defsetting[XIDEN_TRUST_PUBLIC_KEYS (listof well-formed-integrity-info/c)]{
A list of integrity information used to verify public keys. If a
public key fetched for an input passes the integrity check for an
element of @racket[XIDEN_TRUST_PUBLIC_KEYS], then the public key is
considered trustworthy.
}
