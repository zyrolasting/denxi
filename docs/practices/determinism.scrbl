#lang scribble/manual

@require["../shared.rkt" @for-label[@except-in[xiden/pkgdef #%module-begin] racket/base]]

@title[#:tag "determinism"]{Protecting Determinism in Artifacts}

@tech/xiden-reference{Artifacts} that use the same correct integrity
information and signatures are guarenteed to produce the same bytes
from a trusted source, assuming bytes are produced.

This is great for reproducible builds, but handwriting such
information can slow you down. In the below example, changing the
source means changing the integrity information. Changing the
integrity information means changing the signature.

@racketblock[
(artifact (byte-source #"84n\24...")
          (integrity 'sha384 #"47u2c...")
          (signature #"fa9v\0..."
                     #"h!,094..."))
]

One can ease the chore with a remote source.

@racketblock[
(artifact (http-source "https://example.com/my-file")
          (integrity 'sha384 (http-source "https://example.com/my-file.sha384"))
          (signature
           (http-source "https://example.com/public.pem")
           (http-source "https://example.com/my-file.sha384.sig")))
]

This decouples the package definition from content. When an
administrator updates the file, digest, or signature, the package
definition does not have to change. Xiden will still protect the user
from unwanted data by checking trust against public keys,
cryptographic hash functions, and network safety limits. However, the
act of fetching artifact data is no longer deterministic because
servers can go down, or return different responses for the same
request.

If you wish to guarentee determinism, you'll need to convert at least
the integrity information to a hard-coded form. The difference is that
you or your team asserts what bytes are correct, and not an external
source. Lock files are built on this premise.
