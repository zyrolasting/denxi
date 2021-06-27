#lang scribble/manual

@require["../../shared.rkt"
         @for-label[racket/base
                    racket/contract
                    xiden/message
                    xiden/integrity
                    xiden/signature
                    xiden/signature/base
                    xiden/source]
         @for-syntax[xiden/signature]
         xiden/signature]

@title{Signature Checking}

@defmodule[xiden/signature]

@racketmodname[xiden/signature] uses asymmetric cryptography to verify
if a @tech{digest} was signed by a private key. The quality of
signature verification is therefore dependent on the quality of the
@tech{CHF} used to create the digest.


@deftogether[(
@defthing[sourced-signature? flat-contract?]
@defthing[well-formed-signature? flat-contract? #:value (or/c raw-signature? sourced-signature?)]
@defthing[malformed-signature? flat-contract? #:value (not/c well-formed-signature?)]
)]{
Duck typing contracts for @racket[signature] instances.

@racket[sourced-signature?] returns @racket[#t] if at least one of the
fields of the instance is a @tech{source}. This is unlike
@racket[sourced-integrity?], which only checks if the digest field is
a @tech{source}.
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


@defproc[(lock-signature [#:public-key-budget
                          public-key-budget
                          budget/c
                          MAX_EXPECTED_SIGNATURE_PAYLOAD_LENGTH]
                          [#:signature-budget
                           signature-budget
                           budget/c
                           MAX_EXPECTED_SIGNATURE_PAYLOAD_LENGTH]
                          [siginfo well-formed-signature?]
                          [exhaust exhaust/c])
                          signature?]{
Like @racket[lock-integrity] for @racket[signature]s.
}

@defproc[(make-snake-oil-signature [digest bytes?]
                                   [chf-name symbol? (get-default-chf)])
                                   raw-signature?]{
Return a new signature using @racket[snake-oil-private-key].

Do not use in production code.
}

@defthing[MAX_EXPECTED_SIGNATURE_PAYLOAD_LENGTH budget/c]{
An estimated maximum number of bytes (chosen empirically) for a public
key or signature.
}

@defproc[(call-with-snake-oil-cipher-trust [thunk (-> any)]) any]{
Calls @racket[thunk] in tail position.  While control is in the
@racket[thunk], @racket[(XIDEN_TRUST_PUBLIC_KEYS)] is @racket[(list
snake-oil-public-key)].

Implies @racket[call-with-snake-oil-chf-trust].
}

@defsetting*[XIDEN_TRUST_ANY_PUBLIC_KEY]{
@bold{Dangerous}. When true, trust any public key used to verify a signature.
}

@defsetting*[XIDEN_TRUST_UNSIGNED]{
@bold{Dangerous}. When true, trust any input that lacks a valid signature.
}

@defsetting*[XIDEN_TRUST_BAD_SIGNATURE]{
@bold{Dangerous}. When true, trust any input that has a signature that
does not match the input's integrity information.
}

@defsetting[XIDEN_TRUST_PUBLIC_KEYS (listof well-formed-integrity?)]{
A list of integrity information for public keys. Trusts public keys
that can be used to reproduce an element of this list.
}

@define[pk-read-url
"https://www.openssl.org/docs/man1.1.0/man3/PEM_read_bio_PrivateKey.html"]

@include-section{signature/base.scrbl}
@include-section{signature/ffi.scrbl}
@include-section{signature/snake-oil.scrbl}
