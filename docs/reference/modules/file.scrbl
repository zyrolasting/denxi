#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/sequence
                    file/glob
                    denxi/file
                    denxi/subprogram]
          "../../shared.rkt"]

@title{Files}

@defmodule[denxi/file]

@racketmodname[denxi/file] extends and reprovides
@racketmodname[racket/file].

@defproc[(in-paths [pattern (or/c regexp? pregexp? byte-regexp? byte-pregexp? string?)]
                   [start directory-exists? (current-directory)])
         (sequence/c path?)]{
Returns a sequence of paths relative to @racket[start] that match
@racket[pattern].

If @racket[pattern] is a string and not a regular expression object,
then @racket[pattern] is used as a glob pattern for use in
@racket[glob-match?].
}

@defproc[(path-matching [pattern (or/c regexp? pregexp? byte-regexp? byte-pregexp? string?)]
                        [start directory-exists? (current-directory)])
         (subprogram/c path?)]{
Like @racket[in-paths], except this returns a @tech{subprogram} that
fails with @racket[$path-not-found] on the @tech{subprogram log} if no
paths are found.  Otherwise, the procedure uses the first matching
path.
}


@defproc[(directory-empty? [path path-string?]) boolean?]{
Returns @racket[(null? (directory-list path))].
}


@defproc[(something-exists? [path path-string?]) boolean?]{
@racketblock[
(or (file-exists? path)
    (directory-exists? path)
    (link-exists? path))]
}


@defproc[(linked? [link-path path-string?] [path path-string?]) boolean?]{
Returns @racket[#t] if @racket[link-path] refers to an existing link,
a file, directory, or link exists at @racket[path], and both paths
resolve to the same filesystem identifier.
}

@defproc[(file-link-exists? [link-path path-string?]) boolean?]{
Returns @racket[#t] if @racket[link-path] refers to an existing link,
and the link points to a regular file.
}

@defstruct*[($path-not-found $message) ([pattern (or/c regexp? pregexp? byte-regexp? byte-pregexp? string?)] [wrt path-string?])]{
A @tech{message} reporting if @racket[(path-matching pattern wrt)] found no path.
}


@section{Content Addressing}

Denxi addresses arbitrary data using digests. One may override how
data appears when computing new addresses.

@defproc[(make-content-address [path (or/c directory-exists? file-exists? link-exists?)]) bytes?]{
Returns a digest suitable for use in @racket[path-record-digest].

The digest always uses @racket[(get-default-chf)], but the bytes used
to produce the digest depend on @racket[current-content-scanner].
}


@defthing[current-content-scanner (parameter/c (-> path-string? input-port?))]{
A parameter used to store a procedure.

The procedure must accept a @racket[path-string?] and return an input
port. The input port yields bytes that act as representitive content
for whatever exists at that location.

Defaults to @racket[scan-all-filesystem-content].
}


@defproc[(scan-all-filesystem-content [path path-string?]) input-port?]{
Returns an input port that draws all accessible data (including some
platform-specific data) from an existing file, directory, or link at
@racket[path] according to the rules below. The rules are checked in
the order shown.

@bold{Warning:} Large directories and files may take a long time to
process. Only use when change detection is more important than
performance.

@bold{Note:} When this definition refers to a “name”, it means the
bytes in @racket[(string->bytes/utf-8 (~a (file-name-from-path
path)))].

If @racket[(link-exists? path)], then return a port that yields bytes
from the link's name. The link is not followed.

If @racket[(file-exists? path)], then return a port that yields bytes
from the file's name, permissions, and contents.

If @racket[(directory-exists? path)], then return a port that yields
bytes from the directory's name, permissions, and contents. Contents
are processed recursively without tail-call optimization.

Otherwise, raise @racket[($no-content-to-address path)].
}

@defstruct*[($no-content-to-address $message) ([path path-string?])]{
When trying to create a digest to address content, @racket[path] did not
point to an existing file, directory, or link.
}
