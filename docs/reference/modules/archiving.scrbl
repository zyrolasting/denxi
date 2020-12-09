#lang scribble/manual

@require[@for-label[racket/base file/tar racket/contract xiden/archiving]
          "../../shared.rkt"]

@title{Archiving}

@defmodule[xiden/archiving]

@racketmodname[xiden/archiving] provides generic procedures for
combining multiple files into a single file, and vice-versa.

@defproc[(pack [paths (listof path-string?)] [out output-port?]) void?]{
A simplified interface for @racket[tar->output].
}

@defproc[(unpack [in (or/c path-string? input-port?)]) void?]{
Extracts files from @racket[in] with respect to @racket[current-directory].

If @racket[in] is a @racket[path-string?], then @racket[(unpack in)]
is equivalent to @racket[(call-with-input-file in unpack)].

When @racket[in] is an input port, then @racket[unpack] infers the
archive format from the file extension in @racket[(object-name in)].
If this is @racket{.tar}, then @racket[unpack] behaves like
@racket[untar].  If @racket{.tgz} or @racket{.tar.gz}, then
@racket[untgz]. If @racket{.zip}, @racket[unzip].
}
