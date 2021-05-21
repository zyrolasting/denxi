#lang scribble/manual

@title[#:tag "integrity"]{Integrity Checking}

@require[@for-label[racket
                    xiden/codec
                    xiden/integrity
                    xiden/crypto]
                    "../shared.rkt"]

Xiden uses @deftech{cryptographic hash functions}
(@tech/xiden-reference{CHF}s) to turn data into fixed-length byte
strings called @italic{digests}. If two digests match, then we can
reasonably assume they were created using the same content. That is,
unless the CHF has a @italic{collision}, where two blobs produce the
same digest. In the event a CHF is compromised by a researcher or
attacker, you can revoke trust in that CHF by removing it from the
@racket[XIDEN_TRUST_CHFS] setting.

Xiden checks the integrity for some data using the following
expression:

@racketblock[
(require xiden/integrity)
(check-integrity #:trust-bad-digest
                 (XIDEN_TRUST_BAD_DIGEST)
                 #:trust-message-digest-algorithms
                 (XIDEN_TRUST_CHFS)
                 data)]

Since an integrity check is controlled by a
@tech/xiden-reference{runtime configuration}, you can see why it is
part of Xiden's attack surface.


@section{Relationship to OpenSSL}

When you have OpenSSL installed on your system, you can create a
digest using the @litchar{openssl dgst} command.

@verbatim|{
$ openssl dgst -sha3-384 default.tgz
SHA3-384(default.tgz)= 299e3eb744725387e...
}|

There's a nuance: You'll only want to trust the digest if you trust
the input file and the CHF. That's up to you.

Since the OpenSSL command gave us the digest as a hex-string, we can
express it as integrity information in Xiden.

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
(make-trusted-integrity-info (build-path "data.tgz") 'sha384)
]
