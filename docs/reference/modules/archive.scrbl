#lang scribble/manual

@require[
@for-label[racket/base
           racket/contract
           file/untar
           file/untgz
           file/unzip
           racket/contract
           xiden/archive
           xiden/file
           xiden/logged]
           "../../shared.rkt"]

@title{Archives}

@defmodule[xiden/archive]

@racketmodname[xiden/archive] extracts files from a single archive file.

@defproc[(extract [in (or/c path-string? input-port?)]) (logged/c (or/c FAILURE void?))]{

@margin-note{@racket[extract] offers no control over the destination
paths of extracted files. Use @racket[in-paths] or @racket[path-matching]
to select extracted items.}

Returns a @tech{logged procedure} @racketid[P] that extracts files
from @racket[in] with respect to @racket[(current-directory)].

If @racket[in] is a @racket[path-string?], then @racket[(extract in)]
is equivalent to @racket[(call-with-input-file in extract)]. The
rest of this entry assumes @racket[in] is an input port.

If @racketid[P] cannot figure out how to extract files, it will return
@racket[FAILURE] and log @racket[($extract-report 'unsupported
(object-name in))].

If @racketid[P] succeeds, it will return @racket[(void)] and log
@racket[($extract-report 'done (object-name in))].

The extraction process infers the archive format from the file
extension in @racket[(object-name in)].  If this is @racket{.tar},
then @racketid[P] behaves like @racket[untar].  If @racket{.tgz}
or @racket{.tar.gz}, then @racket[untgz]. If @racket{.zip},
@racket[unzip]. Otherwise, @racketid[P] will attempt to use
a @tech{plugin}'s @racket[get-extract-procedure].
}


@defstruct*[$extract-report ([status symbol?] [target any/c])]{
Describes the behavior of a call to @racket[extract].  @racket[target]
is @racket[eq?] to the @racket[object-name] of the input port used in
the corresponding @racket[extract] call.

@racket[status] is

@itemlist[
@item{@racket['unsupported] if there was no known way to extract the archive.}
@item{@racket['done] if the extraction finished successfully.}
]

}
