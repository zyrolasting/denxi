#lang scribble/manual

@title[#:tag "signature"]{Signature Checking}

@require[@for-label[racket
                    xiden/artifact
                    xiden/codec
                    xiden/integrity
                    xiden/signature
                    xiden/source
                    xiden/crypto]
                    "../shared.rkt"]

Read @secref{integrity} first.

Xiden uses
@hyperlink["https://en.wikipedia.org/wiki/Public-key_cryptography"]{asymmetric
cryptography} to authenticate where data comes from.

A provider uses their private key to sign
@tech/xiden-reference{digests} in integrity information. In this
@tech/xiden-reference{artifact}, the @racket[signature] term contains
the public key to verify the adjacent bytes. The signature starting
with the bytes @racket[#"a55"] is derived directly from the bytes
starting with @racket[#"93f"].

@racketblock[
(artifact (http-source "https://example.com/default.tgz")
          (integrity 'sha1 #"93f...")
          (signature (http-source "https://example.com/public-key.pem")
                     #"a55..."))
]

You can perform a signature check programmatically using
@racket[check-signature].


@section{Signing with OpenSSL}

When you have OpenSSL installed on your system, you can create a
Xiden-compatible signature using the following commands.

@verbatim|{
$ >default.tgz.sha1 \
  openssl dgst -binary -sha1 default.tgz

$ <default.tgz.sha1 \
  >default.tgz.sha1.sig \
  openssl pkeyutl -sign -inkey /path/to/private-key
}|

We want the signature from the raw bytes of the digest, which is why
the @litchar{dgst} command uses the @litchar{-binary} flag. The byte
content of the signature created by the second command is suitable for
use in a package definition.
