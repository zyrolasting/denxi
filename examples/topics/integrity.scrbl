#lang scribble/manual

@title[#:tag "integrity"]{Integrity Checking}

@require[@for-label[racket
                    xiden/codec
                    xiden/integrity
                    xiden/crypto]
                    "../shared.rkt"]

Xiden compares @tech/xiden-guide{digests} to check data integrity, but
only if the user trusts the @tech/xiden-guide{cryptographic hash
functions} used to make the digests.

@racketmodname[xiden/integrity] is Xiden's integrity checking
module. You can perform your own integrity check using the provided
@racket[check-integrity] function.  You tell Xiden what CHF
implementations are trusted (and therefore available) using
@racket[current-chfs]. @racket[XIDEN_TRUST_CHFS] is a simplified
setting that adds built-in CHF implementations to
@racket[current-chfs].

To create integrity information in Xiden, create an instance of
@racket[integrity] using a symbolic name of a CHF, and a byte string
for a digest. The value represents a claim that some content can
reproduce the same digest using the named CHF.

@racketblock[
(require xiden/integrity)

(integrity 'sha3-384 #"...")
]

I replaced a realistic example digest with @racket{#"..."}  because
digests can be long. You can abbreviate the byte string with different
encodings.

@racketblock[
(require xiden/codec xiden/integrity)

(integrity 'sha1 (hex "af9..."))
(integrity 'md5 (base64 "QZ9l..."))
]
