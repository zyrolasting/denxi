#lang scribble/manual

@require["../shared.rkt"
         @for-label[racket/base
                    racket/contract
                    xiden/message
                    xiden/integrity
                    xiden/signature]]

@title{Signature Checking}

@defmodule[xiden/signature]

@racketmodname[xiden/signature] authenticates data providers
through their digital signatures.

@section{Signature Verification Model}

@|project-name|'s signature checking process is equivalent to the
following OpenSSL command:

@verbatim|{$ openssl pkeyutl -verify -sigfile SIGFILE -pubin -inkey PUBKEY <DIGEST}|

Specifically, it reads @italic{unencoded} bytes for a message digest
(a.k.a. content hash) and verifies a signature from a signature file
(@tt{SIGFILE}) and a public key file (@tt{PUBKEY}).

This implies that @project-name accepts any format OpenSSL does for
the signature and public key, but the cipher algorithm used to create
the signature must support the message digest algorithm used to create
the digest.

If you create a signature and cannot seem to pass the signature check,
make sure that you are actually signing the raw bytes for a message
digest because that's what @|project-name| checks against. If you use
a command like @litchar{openssl dgst -sign ...}, then you would be
signing an ANS.1-encoded digest. Use @litchar{openssl pkeyutl -binary
...} instead.


@section{Signature Check Bindings}

@defthing[siginfo-variant/c flat-contract? #:value (or/c bytes? string?)]{
In the context of signature checking, a public key or signature
can be expressed using a variant value type.

A byte string is used as-is for verfication. This allows you to pass a
public key or signature as raw bytes directly into a signature check.

@racketblock[(signature-info #"-----BEGIN PUBLIC KEY-----\nMIIBIjAN..." #"*&\2...")]

A Racket string is instead treated as a @tech{source}, so that the
bytes can be fetched from elsewhere.

@racketblock[(signature-info "https://example.com/public.pem" (from-file "local.sign"))]
}

@defstruct*[signature-info ([pubkey siginfo-variant/c] [body siginfo-variant/c])]{
Holds an expression of a public key file and the bytes of a signature
created using a private key.
}

@defproc[(signature [pubkey siginfo-variant/c] [body siginfo-variant/c]) signature-info?]{
An abbreviated @racket[signature-info] constructor for use in @tech{package definitions}.
}

@defproc[(bind-trusted-public-keys [trusted-keys (listof well-formed-integrity-info/c)]) (-> path-string? boolean?)]{
Returns a procedure @racket[P], such that @racket[(P
"/path/to/key.pem")] (for example) is @racket[#t] if the given key
passes an @tech{integrity check} for one of the
@racket[integrity-info] structures in @racket[trusted-keys].

This is used internally to determine a user's trust in a key
before using a @tech{package input}.
}

@defthing[well-formed-signature-info/c flat-contract?]{
Recognizes an instance of @racket[signature-info] that is suitable for use in signature checking.
}

@defproc[(get-public-key-path [variant (or/c string? bytes?)]) path?]{
Returns a @tech{workspace}-relative path to a public key file. The file may be cached.
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

@racket[stage] is a symbol for tracing the source of corresponding
instance.

@racket[public-key-path] is a path to a cached public key file
used for a check, or @racket[#f] if a public key is not relevant.
}
