#lang scribble/manual

@title[#:tag "integrity"]{Integrity Checking}

@require[@for-label[racket
                    xiden/codec
                    xiden/integrity
                    xiden/crypto]
                    "../shared.rkt"]

We need confidence that data has good integrity, meaning that it
wasn't tampered with during transmission.

Xiden uses @deftech{cryptographic hash functions}
(@tech/xiden-reference{CHF}s) to turn data into fixed-length byte
strings called @italic{digests}. If two digests match, then we can
reasonably assume they were created using the same content.

It's possible for a CHF to have a @italic{collision}, where different
data maps to the same digest. In the event a CHF is compromised by a
researcher or attacker, you can revoke trust in that CHF.

Xiden checks the integrity for some data using
@racket[check-integrity].


@section{Relationship to OpenSSL}

If you have an @litchar{openssl} binary, you can use it to create a
digest.

@margin-note{@litchar{-sha3-384} might not be available on your
system, which will make @litchar{openssl dgst} complain that it
doesn't recognize the option. The error should show you a command to
list supported CHFs that you can use instead.}

@verbatim|{
$ openssl dgst -sha3-384 my-file
SHA3-384(default.tgz)= 299e3eb744725387e...
}|

You'll only want to trust the digest if you trust the input file and
the CHF. Since the OpenSSL command gave us the digest as a hex string,
we can express it as integrity information in Xiden.

@racketblock[
(require (only-in xiden/codec hex)
         (only-in xiden/integrity integrity))
(integrity 'sha3-384 (hex "299e3eb744725387e..."))]


@section{Creating Integrity Information using Xiden}

An external tool might use a different encoding and algorithm than
what Xiden supports.  If you want the entire integrity expression with
options supported by Xiden, then use the @litchar{xiden mkint}
command. This example does the same thing, except you'll get an entire
integrity expression as output.

@verbatim|{
$ xiden mkint sha384 hex default.tgz
(integrity 'sha384 (hex "299e3eb744725387e..."))
}|

Alternatively, you can tell Xiden to read from standard input.
Just use a dash in place of the file.

@verbatim|{
$ <default.tgz xiden mkint sha384 hex -
(integrity 'sha384 (hex "299e3eb744725387e..."))
}|

If preferred, you can programatically construct integrity information
from trusted data using @racket[make-trusted-integrity-info].

@racketblock[
(require xiden/integrity)
(make-trusted-integrity (build-path "data.tgz") 'sha384)
]
