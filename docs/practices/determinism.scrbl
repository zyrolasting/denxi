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

One can ease the chore using remote sources.

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

The first step towards determinism is hosting a local copy of at least
the integrity information. This does not make the build fully
reproducible because content and signature infromation might still be
unavailable.  Local integrity still makes a difference because you or
your team asserts what bytes are correct, and not an external
source. Lock files are built on this premise for other dependency
managers.

Lock files are not necessary when you leverage Xiden's design, which
caches all fulfilled data. When you install software starting from
zero-trust, a successful installation implies that your system
contains an integrous copy of authenticated data. You can either check
your @tech/xiden-reference{workspace} into source control or archive
it for distribution as a file. You may need to sign the archive or
commit for others to decide if they trust your distribution. When the
workspace is sent to another user's system, the links may be invalid,
but the data will be locally present. This means that when a user
installs package definitions, outputs will be reused normally. On the
other hand, collecting garbage before installing anything will delete
everything in that workspace unless symbolic links are preserved in
the transfer.
