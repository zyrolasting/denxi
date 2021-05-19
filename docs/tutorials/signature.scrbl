#lang scribble/manual

@title[#:tag "signature"]{Signature Checking}

@require[@for-label[racket
                    xiden/codec
                    xiden/integrity
                    xiden/signature
                    xiden/openssl]
                    "../shared.rkt"]

This tutorial assumes you have read and understand @secref{integrity}.

Xiden authenticates those who distribute package inputs using
@hyperlink["https://en.wikipedia.org/wiki/Public-key_cryptography"]{asymmetric
cryptography}.

Specifically, Xiden requires a provider to use their private key to
sign digests used in integrity information. Let's say you write a
package input similar to the following:

@racketblock[
(input "default.tgz"
       (artifact (http-source "https://example.com/default.tgz")
                 (integrity 'sha1 #"93f...")
                 (signature (http-source "https://example.com/public-key.pem")
                            #"a55...")))
]

The @racket[signature] term contains a @tech/xiden-reference{source}
for the public key to verify the adjacent signature, and a
@tech/xiden-reference{source} of the signature itself.

When checking signatures, Xiden requires a signature to certify the
@italic{raw bytes} of the digests used in the artifact's integrity
information. This means that digest's bytes cannot be encoded.  If you
have an encoded digest, you can pass it to one of Xiden's abbreviated
decoding procedures like @racket[hex] or @racket[base64].

But for the sake of the above hypothetical example, we assume that the
signature @racket[#"a55..."] is derived directly from the bytes
@racket[#"93f..."].

You can perform a signature check against the artifact
programmatically using @racket[check-signature].


@section{Signing with OpenSSL}

When you have OpenSSL installed on your system, you can create a
signature using the following commands.

@verbatim|{
$ >default.tgz.sha1 \
  openssl dgst -binary -sha1 default.tgz

$ <default.tgz.sha1 \
  >default.tgz.sha1.sig \
  openssl pkeyutl \
          -sign \
          -inkey /path/to/private-key \
          -passin /path/to/private-key-pass
}|

There are shorter ways to do this
(e.g. @racket[make-signature-bytes]), but this particular form is
important because of a nuance. We want the signature from the raw
bytes of the digest, which is why the @litchar{dgst} command uses the
@litchar{-binary} flag.

The byte content of the signature created by the second command is
suitable for use in a package definition.
