#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/format
                    racket/string
                    xiden/artifact
                    xiden/dig
                    xiden/dig/filesystem
                    xiden/message
                    xiden/integrity
                    xiden/openssl
                    xiden/query
                    xiden/signature
                    xiden/source
                    xiden/string]]

@title{Filesystem Shovels}

@defmodule[xiden/dig/filesystem]

You can bind @tech{shovels} to filesystem directories.  In observance
of the @tech{digsite metaphor}, the shovels defined herein always
follow symbolic links.

@defproc[(make-filesystem-shovel [directory-path complete-path?]
                                 [chf md-algorithm/c]
                                 [public-key-source source-variant?])
                                 shovel/c]{
Returns a procedure @racket[S], which attempts to produce
@tech{artifacts} from files in @racket[directory-path]. For an
application @racket[(S relative-path)], the below invariants must hold
to produce an artifact. If any invariant is not met, then @racket[(S
relative-path)] is equivalent to @racket[(broken-shovel
relative-path)].

@itemlist[
@item{@racket[relative-path] complies with @racket[(and/c path-string? (not/c complete-path?))]}
@item{@racket[(build-path directory-path relative-path)] must exist as a readable file, or as a link to a readable file.}
]

For example, assume @litchar{/tmp/example} exists as a file in a
Unix-like system for a Xiden process to read. From here you can bind a
dig procedure to @litchar{/tmp} like so.

@racketblock[
(define S (make-filesystem-shovel "/tmp" 'md5 public-key-source))
]

From here, @racket[(S "example")] will produce an artifact based on
the existing file.

@racketblock[
(artifact-info (file-source (string->path "/tmp/example")) #f #f)
]

If a file does not exist, then @racket[S] behaves like
@racket[broken-shovel]. Note that @racket[S] is non-deterministic, and
may begin returning artifacts depending on the state of the directory
at the time.

We specified @racket['md5] as an example CHF. To add integrity
information for @litchar|{/tmp/example}|, create a
@litchar|{/tmp/example.md5}|. It must hold the @bold{unencoded} bytes
of the digest produced from the contents of @litchar|{/tmp/example}|.
If you do this, then @racket[(dig "example")] will produce this
enhanced artifact.

@racketblock[
(artifact-info
  (file-source (string->path "/tmp/example"))
  (integrity-info 'md5
                  (file-source (string->path "/tmp/example.md5")))
  #f)
]

Formally, integrity information appears in the artifact
when @racket[(build-path directory-path (~a relative-path "."  chf))]
exists as a readable file.

Singature information may appear in another adjacent file.  The
complete directory listing should now appear as follows:

@verbatim|{
/tmp/example
/tmp/example.md5
/tmp/example.md5.sig
}|

@litchar|{/tmp/example.md5.sig}| must hold the @bold{unencoded} bytes
for a signature produced from the @bold{digest}'s contents.  That is,
for @litchar|{/tmp/example.md5}|.

In this scenario, @racket[(dig "example")] will produce a complete
artifact.

@racketblock[
(artifact-info
 (file-source (string->path "/tmp/example"))
 (integrity-info 'md5
                 (file-source (string->path "/tmp/example.md5")))
 (signature-info public-key-source
                 (file-source (string->path "/tmp/example.md5.sig"))))
]


Formally, signature information appears in the artifact when
@racket[(build-path directory-path (~a relative-path "." chf
".sig"))] exists as a readable file.
}


@defproc[(make-filesystem-shovel/pkgdef [directory-path complete-path?]
                                        [chf md-algorithm/c]
                                        [defaults package-query-defaults-implementation/c default-package-query-defaults])
                                        dig/c]{
Like @racket[make-filesystem-shovel], but specialized for files
containing @tech{package definitions}.

For a given provider @racketid[P], package name @racketid[K], edition
@racketid[E], revision name @racketid[A], and revision number
@racketid[N], the following invariants must hold for the files in
@racket[directory-path].

@itemlist[

@item{
If @racket[(build-path directory-path P "public-key")] exists, it must
be a readable file containing the public key for @racketid[P].
@bold{Corollary:} @racketid[K] must not be @racket[equal?] to
@racket{public-key}.
}

@item{
@racket[file-name-string?] returns @racket[#t] for @racketid[P],
@racketid[K], @racketid[E], @racketid[A], and @racketid[N].
}

@item{
If @racket[(build-path directory-path P K E)] exists, it is a readable
directory suitable for use with @racket[make-filesystem-shovel].
}

@item{
If @racket[(build-path directory-path P K E N)] exists, it is a
readable file containing a @tech{package definition}.
}

@item{
If @racket[(build-path directory-path P K E A)] exists, then it must
be a readable link pointing to @italic{exactly} @racketid[N]. The
link's name must be a @tech{revision name} from the corresponding
package definition file (Digest and signature links are not required).
}

]

In terms of the above, here is a valid directory structure for a
catalog.

@verbatim{
~/xiden-catalog
├── alice
│   ├── default -> renderer
│   ├── public-key
│   ├── raw-input
│   │   └── default
│   │       ├── 0
│   │       ├── 0.sha3-384
│   │       ├── 0.sha3-384.sig
│   │       ├── 1
│   │       ├── 1.sha3-384
│   │       ├── 1.sha3-384.sig
│   │       ├── 5
│   │       ├── 5.sha3-384
│   │       ├── 5.sha3-384.sig
│   │       ├── open-beta -> 5
│   │       └── xbox-controller-support -> 1
│   └── renderer
│       ├── default -> vulkan
│       ├── directx
│       │   ├── 0
│       │   ├── 0.sha3-384
│       │   └── 0.sha3-384.sig
│       └── vulkan
│           ├── 0
│           ├── 0.sha3-384
│           ├── 0.sha3-384.sig
│           └── default -> 0
├── default -> alice
└── john
    ├── calculator
    │   ├── default
    │   │   ├── 0
    │   │   ├── 0.sha3-384
    │   │   ├── 0.sha3-384.sig
    │   │   ├── 1
    │   │   ├── 1.sha3-384
    │   │   ├── 1.sha3-384.sig
    │   │   ├── 2
    │   │   ├── 2.sha3-384
    │   │   ├── 2.sha3-384.sig
    │   │   ├── initial -> 0
    │   │   └── post-feedback -> 2
    │   └── scientific
    │       ├── 0
    │       ├── 0.sha3-384
    │       ├── 0.sha3-384.sig
    │       ├── 1
    │       ├── 1.sha3-384
    │       ├── 1.sha3-384.sig
    │       └── zero-day-patch -> 1
    ├── calendar
    │   ├── chinese
    │   │   ├── 0
    │   │   ├── 0.sha3-384
    │   │   └── 0.sha3-384.sig
    │   └── gregorian
    │       ├── 0
    │       ├── 0.sha3-384
    │       └── 0.sha3-384.sig
    └── public-key
}

In this example, the providers are Alice, a video game developer, and
John, an office application developer.

There is a gap in revision numbers in Alice's @tt{raw-input} package.
This is fine, because it reflects the possibility of missing
information.  The catalog will return the latest available match for
queries like @litchar{alice:raw-input:::open-beta}.

Finally, each directory may contain a symlink named after a string one
could use in a package query. You can leverage this to support getting
the latest available version using queries like
@litchar{alice:raw-input}.
}
