#lang denxi/document

@title{Filesystem Shovels}

@defmodule[denxi/dig/filesystem]

You can bind @tech{shovels} to filesystem directories.  In observance
of the @tech{digsite metaphor}, the shovels defined herein always
follow symbolic links.

@defproc[(make-digest-file-path [path path-string?] [chf symbol?]) path?]{
Returns @racket[(~a path "." chf)]

Used as a conventional location for unencoded digests for whatever is
located at @racket[path].
}

@defproc[(make-signature-file-path [path path-string?]) path?]{
Returns @racket[(~a path ".sig")]

Use as a conventional location for unencoded signatures.
}

@defproc[(make-filesystem-shovel [directory-path complete-path?]
                                 [chf symbol?]
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
Unix-like system for a Denxi process to read. From here you can bind a
dig procedure to @litchar{/tmp} like so.

@racketblock[
(define S (make-filesystem-shovel "/tmp" 'md5 public-key-source))
]

From here, @racket[(S "example")] will produce an artifact based on
the existing file.

@racketblock[
(artifact (file-source (string->path "/tmp/example")) #f #f)
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
(artifact
  (file-source (string->path "/tmp/example"))
  (integrity 'md5
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
(artifact
 (file-source (string->path "/tmp/example"))
 (integrity 'md5
            (file-source (string->path "/tmp/example.md5")))
 (signature public-key-source
            (file-source (string->path "/tmp/example.md5.sig"))))
]


Formally, signature information appears in the artifact when
@racket[(build-path directory-path (~a relative-path "." chf
".sig"))] exists as a readable file.
}
