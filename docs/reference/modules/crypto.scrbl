#lang scribble/manual

@title{Cryptography}

@require[@for-label[racket/base
                    racket/contract
                    racket/pretty
                    xiden/crypto]
         racket/pretty
         @for-syntax[xiden/crypto]
         xiden/crypto
         "../../shared.rkt"]

@defmodule[xiden/crypto]

@racketmodname[xiden/crypto] is a high-level interface for a bundled
@tt{libcrypto} extension. Error information may require knowledge of
low-level operations, because breakages in the underlying library are
critical failures in the Xiden runtime.


@section{Cryptographic Hash Functions}

@racketmodname[xiden/crypto] defines supported @deftech{cryptographic
hash functions} or (@deftech{CHF}s) based on the underlying
@tt{libcrypto} build.

@defthing[DEFAULT_CHF chf/c]{
A cryptographic hash function considered to be collision resistant for
the current Xiden installation.

This value is subject to change for security reasons.
}

@defthing[chf/c flat-contract?]
Returns @racket[#t] if the argument is an element of @racket[cryptographic-hash-functions].
}

@defthing[cryptographic-hash-functions (listof symbol?)]{
A list of symbols that represent supported @tech{cryptographic hash functions}.

Bound to @typeset-code[(pretty-format #:mode 'print cryptographic-hash-functions)]
}


@section{High-Level Crypto API}

@bold{All procedures in this section may @racket[raise] a
@racket[$crypto] message instead of returning a value.}

@defproc[(make-digest [in (or/c bytes? path-string? input-port?)]
                      [chf chf/c DEFAULT_CHF])
                      bytes?]{
Returns the unencoded bytes for a message digest computed using
@racket[chf], using bytes drawn from a given input port.

If @racket[in] is a path string, it is coerced to an input port using
@racket[call-with-input-file*].

If @racket[in] is a byte string, it is coerced to an input port using
@racket[open-input-bytes].
}

@define[pk-read-url
"https://www.openssl.org/docs/man1.1.0/man3/PEM_read_bio_PrivateKey.html"]

@defproc[(make-signature [digest bytes?]
                         [chf chf/c]
                         [private-key bytes?]
                         [private-key-password (or/c #f bytes?) #f])
                         bytes?]{
Returns unencoded bytes for a signature. The signature applies to a
digest computed using @racket[(make-digest digest chf)]. The private
key and its password (if any) are provided to the underlying
@hyperlink[pk-read-url]{@tt{PEM_read_bio_PrivateKey}} function in
@tt{libcrypto}.

@bold{Caveat:} @racket[make-signature] expects cleartext secrets,
which means it should only be used as the final step of a secured
process using trusted software. Take care with where and how long the
@racketfont{private-*} arguments remain in memory. Also be careful
with how you ask users to provide the secrets to load into
memory. OpenSSL binaries typically advise users to store them in files
to avoid leaking them in monitoring utilities like @tt{ps} or
@tt{htop}.

@bold{Caveat:} @racket[private-key] must be a PEM-encoded text file,
but the key type may be anything supported by the underlying
@tt{libcrypto} build. Whether the operation succeeds depends on
whether the cipher algorithm implied by the key type supports signing
and verification, and is compatible with @racket[chf].
@racket[make-signature] does not verify these conditions, but will
communicate these errors by raising a @racket[$crypto] instance.
}

@defproc[(verify-signature [digest bytes?]
                           [chf chf/c]
                           [signature bytes?]
                           [public-key bytes?])
                           boolean?]{
Returns @racket[#t] if @tt{libcrypto} confirms that the private key
corresponding the the given public key has signed @racket[digest].
For verification to pass, the value bound to @racket[signature] must
be produced using @racket[make-signature], the value bound to
@racket[digest] must be produced using @racket[make-digest] and
@racket[chf], and the public key must be 

}


@deftogether[(
@defthing[snake-oil-public-key bytes?]
@defthing[snake-oil-private-key bytes?]
@defthing[snake-oil-private-key-password bytes?]
)]{
Intentionally-leaked RSA keypair in PEM-format, including password.  The
private key must be unlocked with the password. Do not use in production.
}

@defproc[(sign-with-snake-oil [digest bytes?] [chf chf/c DEFAULT_CHF]) bytes?]{
Signs the given digest (presumably produced using @racket[chf]) using
an intentionally-leaked private key. Do not use in production.
}

@defproc[(get-crypto-error-strings [variant (or/c $crypto:error?
                                                  (listof exact-positive-integer?))])
                                   (listof string?)]{
Like @racket[map], where each element of the input list (accessed using
@racket[$crypto:error-queue], if needed) is replaced by an error
strings fetched from the underlying C library.

This procedure operates independently of @racketmodname[xiden/l10n].
}


@section{Cryptography Failure Messages}

@defstruct*[$crypto ()]{
A @tech{message} about a cryptographic operation.
}

@defstruct*[($crypto:error $crypto)
            ([queue (listof exact-integer?)])]{
A low-level cryptographic option failed. @racket[queue] holds all
error codes on the @tt{libcrypto} error queue at the time the instance
was constructed. In the unlikely event of @racket[(null?
queue)], a context trace may be necessary to interpret the root cause.
}
