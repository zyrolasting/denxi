#lang scribble/manual

@title{Prerequisite Knowledge}

@require[@for-label[racket
                    xiden/integrity]
         "../shared.rkt"]

If you download a file from the Internet, how do you know it's
actually the file you wanted and not a fake? We check data integrity
using @deftech{cryptographic hash functions}, or @deftech{CHF}s. A CHF
turns the file we're worried about into a fixed-length value called a
@deftech{message digest}.  You may have also heard that byte string
called a @deftech{digest}, @deftech{checksum}, or @deftech{hash}. All
terms refer to the output of a CHF in Xiden's documentation.

@(define release-page (hyperlink "https://download.racket-lang.org/releases/8.1/" "Racket's release page"))

Sometimes download links have a “SHA-256” or some such name by a
digest. @|release-page| uses SHA-1 and shows digest as hex strings. If
you download one of those files, you know the file on your disk is
@italic{correct} if it creates the digest the download page said it
would.

@verbatim|{
$ wget -O install-racket.exe \
    https://download.racket-lang.org/releases/8.1/installers/racket-minimal-8.1-i386-win32-bc.exe

# This digest matches the one listed on the page, so the file is correct.
$ openssl dgst -sha1 install-racket.exe
SHA1(install-racket.exe)= 78e19d25cb2a26264aa58771413653d9d8b5a9dc
}|

When digests match, we have confidence in the installers integrity so
long as we trust the
CHF. @hyperlink["https://www.schneier.com/blog/archives/2020/01/new_sha-1_attac.html"]{Smart
people broke SHA-1}, so our collective trust in CHFs changes over time.

The word “cryptographic” in “cryptographic hash function” carries a
lot of weight. If a CHF works well, it is hard for your arch-nemesis
to create a fake version of a program with the same digest.

Okay, so you got the file you want, but how do you know the file came
from someone you trust? After all, if your nemesis fools you into
trusting their own digests, then a CHF isn't good enough. For this we
use @deftech{asymmetric cryptography}, which gives us a way to not
only hide data from evesdroppers, but also sign data so that we know
it came from somebody we trust.

For this we have two keys. One is public, so everyone knows it. The
other is known only to you. The public key can scramble a message such
that only you can reveal it. It's hard to think of two keys
interacting because we normally think of a key and a lock. Only the
private key actually “opens” anything, so people normally explain this
with a lockbox or mailbox metaphor. I can't think of a way to phrase
it that wouldn't look like plagarism, so I'll let you Google that.

The point is that this only works if your private key stays a secret,
and we know it was you who shared your public key. There are
@hyperlink["https://en.wikipedia.org/wiki/Key_signing_party"]{gatherings
for that}.

Unlike the rest of this reference, I'm allowing myself to be extremely
loose with my language here. You will need at least a working
knowledge of what Xiden is doing to interpret some parts of the
reference.
