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

@project-name verifies signature information using a multi-stage
process that considers @tech{runtime configuration} and the available
information. A high-trust configuration may skip signature
verification entirely, or perform a signature verification with
implicit trust in a public key. Both are ill-advised, but can be
tracked if used. The output of the signature check is a
@racket[$signature] @tech{message}, which reports if the check passed,
and at what stage the verification process stopped.


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
passes an integrity check for one of the @racket[integrity-info]
structures in @racket[trusted-keys].

This is used internally to determine a user's trust in a key
before using a @tech{package input}.
}

@defthing[well-formed-signature-info/c flat-contract?]{
Recognizes an instance of @racket[signature-info] that is suitable for use in signature checking.
}

@defproc[(get-public-key-path [variant (or/c string? bytes?)]) path?]{
Returns a @tech{workspace}-relative path to a public key file. The file may be cached.
}

@defproc[(check-signature [digest bytes?] [public-key-path path-string?] [signature-variant siginfo-variant/c]) boolean?]{
Returns @racket[#t] if the public key located at the given path
verifies the signature against the @racket[digest].
}

@section{Authentication Factors}

The @racketmodname[xiden/signature] procedures that start with
@racket[consider-*] each offer exactly one check regarding
authentication. They can be used independently of each other, but a
full signature check requires use of every procedure. Failure to pass
control through any procedure is the same as implicitly trusting
whatever was not checked.

Each procedure uses continuation-passing style to advance to other
checks, but may instead return a @racket[$signature] instance to
finish authentication.

This approach aids testing security-critical points of the program,
and aids auditing efforts in field use.

@defstruct*[($signature $message) ([ok? boolean?]
                                   [stage symbol?]
                                   [public-key-path (or/c #f path-string?)])
                                  #:prefab]{
A @tech{message} that reports the results of a signature check. Given
an instance @racket[S], @racket[($signature-ok? S)] is @racket[#t] if
the check passed.

@racket[($signature-stage S)] is @racket[eq?] to the
@racket[object-name] of the @racketid[consider-*] procedure used to
conclude the check. Note for auditing purposes that any other stage
indicates a higher trust (and thus less secure) @tech{runtime
configuration}.

@racket[($signature-public-key-path S)] is a path to a public key file
used for a check, or @racket[#f] if a public key is not relevant for
the @racket[stage].
}

@defproc[(consider-integrity-trust [#:trust-bad-digest trust-bad-digest any/c]
                                   [siginfo any/c]
                                   [continue (-> any/c any)]) any]{
Returns @racket[($signature #t (object-name consider-integrity-trust)
#f)] if @racket[trust-bad-digest] is true. Otherwise, returns
@racket[(continue siginfo)].

@bold{Premise}: If a user is willing to trust bytes regardless of the
results of an @tech{integrity check}, then a signature check offers no
added value.
}

@defproc[(consider-unsigned [#:trust-unsigned trust-unsigned any/c]
                            [siginfo any/c]
                            [continue (-> any/c any)]) any]{
Returns @racket[($signature #t (object-name consider-unsigned) #f)] if
@racket[trust-unsigned] is true, and @racket[siginfo] is not
well-formed. Otherwise, returns @racket[(continue siginfo)].

@bold{Premise}: It is a provider's responsibility to provide a
well-formed signature for their data. Failing to do so is considered
equivalent to not signing the data at all.
}

@defproc[(consider-public-key-trust [#:trust-public-key? trust-public-key? (-> path-string? any/c)]
                                    [#:public-key-path public-key-path path-string?]
                                    [siginfo well-formed-signature-info/c]
                                    [continue (-> path-string? well-formed-signature-info/c any)]) any]{
Returns @racket[($signature #f (object-name consider-public-key-trust)
public-key-path)] if @racket[(trust-public-key? public-key-path)] is
@racket[#f].  Otherwise, returns @racket[(continue public-key-path
siginfo)].

@bold{Premise}: Nothing stops a stranger from publishing someone
else's public key, so a user must vet public keys on their own
and provide affirmative consent to use any particular key for
authentication.
}

@defproc[(consider-signature-info [public-key-path path-string?]
                                  [intinfo well-formed-integrity-info/c]
                                  [siginfo well-formed-signature-info/c]) $signature?]{
Returns @racket[($signature pass? (object-name
consider-signature-info) public-key-path)], where @racket[pass?] is
bound to the result of a @racket[check-signature] call.

This is the last possible procedure used out of the
@racketid[consider-*] procedures.
}
