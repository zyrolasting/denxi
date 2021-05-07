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

@racketmodname[xiden/signature] authenticates data providers
through their digital signatures.

@section{Signature Verification Model}

Xiden's signature checking process is equivalent to the
following OpenSSL command:

@verbatim|{$ openssl pkeyutl -verify -sigfile SIGFILE -pubin -inkey PUBKEY <DIGEST}|

Specifically, it reads @italic{unencoded} bytes for a message digest
(a.k.a. content hash) and verifies a signature from a signature file
(@tt{SIGFILE}) and a public key file (@tt{PUBKEY}).

This implies that Xiden accepts any format OpenSSL does for the
signature and public key, but the cipher algorithm used to create the
signature must support the message digest algorithm used to create the
digest. Xiden does not check if this invariant holds, and will forward
any errors raised by OpenSSL.

If you create a signature and cannot seem to pass the signature check,
make sure that you are actually signing the raw bytes for a message
digest, not an encoded form of that digest. For example, if you use a
command like @litchar{openssl dgst -sign ...}, then you would be
signing an ANS.1-encoded digest. Use @litchar{openssl pkeyutl -binary
...}  instead.

In the event a cipher algorithm is compromised, then Xiden will adapt
by using different OpenSSL commands. The interface documented in this
section is not expected to change for that reason.


@section{Signature Check Bindings}

@defstruct*[signature-info ([pubkey source-variant?] [body source-variant?])]{
Holds an expression of a public key file and the bytes of a signature
created using a private key.
}

@defproc[(signature [pubkey source-variant?] [body source-variant?]) signature-info?]{
An abbreviated @racket[signature-info] constructor for use in @tech{package definitions}.
}

@defthing[well-formed-signature-info/c flat-contract?]{
Recognizes an instance of @racket[signature-info] that is suitable for use in signature checking.
}

@defproc[(fetch-signature-payload [src source-variant?] [exhaust exhaust/c]) any/c]{
Like @racket[fetch], except the return value is a path to a possibly
cached file of limited size, or the value returned from
@racket[exhaust].

In practice, the file is expected to contain either a public key or a
signature.  In any case, the file is expected to respect the
invariants of the OpenSSL command documented at the beginning of this
section.
}


@defthing[current-verify-signature (parameter/c (-> integrity-info? signature-info? boolean))]{
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


@defproc[(check-signature [#:trust-public-key? trust-public-key? (-> path-string? any/c)]
                          [#:public-key-path public-key-path path-string?]
                          [#:trust-unsigned trust-unsigned any/c]
                          [#:trust-bad-digest trust-bad-digest any/c]
                          [siginfo (or/c #f signature-info?)]
                          [intinfo well-formed-integrity-info/c]) $signature?]{
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

If @racket[(trust-public-key? public-key-path)] is true, both
@racket[siginfo] and @racket[intinfo] are well-formed, and the public
key verifies the signature against the integrity information, the check
passes.

The check fails in all other conditions.
}

@defstruct*[($signature $message) ([ok? boolean?]
                                   [stage symbol?]
                                   [public-key-path (or/c #f path-string?)])
                                  #:prefab]{
A @tech{message} that reports the results of a signature check.

@racket[ok?] is @racket[#t] if the check passed.

@racket[stage] is a symbol representing the procedure responsible for
the check result.

@racket[public-key-path] is a path to a cached public key file
used for a check, or @racket[#f] if a public key is not relevant.
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
