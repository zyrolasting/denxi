#lang scribble/manual

@title{Prerequisite Knowledge}

@require[@for-label[racket
                    xiden/integrity]
         "../shared.rkt"]

To understand this reference, you should have a working understanding
of @deftech{cryptographic hash functions}, or @deftech{CHF}s. A CHF
turns a variable-length value into a fixed-length value called a
@deftech{message digest}, a.k.a. @deftech{digest}, @deftech{checksum},
or @deftech{hash}. All terms refer to the output of a CHF in Xiden's
documentation.

Sometimes download links have a “SHA-256” or some such name by a
digest. Before version 8.2, Racket installers come with with SHA-1
digests as hex strings. If you download one of those installers, you
should create your own digest using SHA-1 using the installer's
contents.  If the digests match, you have reason to believe the
installer has not been altered in transit. In other words, it
maintained its integrity.

@verbatim|{
$ wget -O install-racket.exe \
    https://download.racket-lang.org/releases/8.1/installers/racket-minimal-8.1-i386-win32-bc.exe

# This digest matches corresponding digest on the 8.1 releases page, so the file is correct.
$ openssl dgst -sha1 install-racket.exe
SHA1(install-racket.exe)= 78e19d25cb2a26264aa58771413653d9d8b5a9dc
}|

There's a caveat. Even when digests match, we can only assume the
installers has good integrity if we trust the
CHF. @hyperlink["https://www.schneier.com/blog/archives/2020/01/new_sha-1_attac.html"]{Smart
people induced a collision in SHA-1}, meaning that it is possible to
trick you into thinking that a harmful file is safe when you check
integrity using SHA-1. This is why the word “cryptographic” in
“cryptographic hash function” carries a lot of weight. If a CHF works
well, it is hard to reproduce a known digest with doctored content.

When CHFs aren't good enough, Xiden uses @deftech{asymmetric
cryptography} to verify that a file came from a trusted party.
Assymetric cryptography involves two keys. One key is public, and the
other is private (that is, known only by you). The public key can
scramble a message such that only the private key holder can read it.

Private keys can also sign data, such that only the corresponding
public key can verify that the holder of the private key created the
signature. That way, if you trust the private key holder, you can
trust the signed data. Xiden verifies signatures this way.

All of this only works if your private key stays secret, and we know
it was you who shared your public key. There are
@hyperlink["https://en.wikipedia.org/wiki/Key_signing_party"]{gatherings
for that}, if you are inclined to attend.

As you read this reference, you can assume that any abstractions that
mention CHFs or signatures are intimately tied to Xiden's trust model.
