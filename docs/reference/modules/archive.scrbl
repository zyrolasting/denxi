#lang scribble/manual

@require[
@for-label[racket/base
           racket/contract
           file/untar
           file/untgz
           file/unzip
           racket/contract
           denxi/archive
           denxi/input
           denxi/file
           denxi/subprogram]
           "../../shared.rkt"]

@title{Archives}

@defmodule[denxi/archive]

@racketmodname[denxi/archive] extracts files from a single archive file.


@defthing[current-find-extract-procedure
          (parameter/c (-> path-string?
                           (or/c #f (-> input-port? any))))]{
A parameter used to find an extraction procedure for an archive
located at a given path. The procedure in the parameter may return
@racket[#f] if no other procedure is available.

The default value always returns @racket[#f].
}


@defproc[(extract [in (or/c path-string? input-port?)])
                  (subprogram/c void?)]{
@margin-note{@racket[extract] offers no control over the destination
paths of extracted files. Use @racket[in-paths] or
@racket[path-matching] to select extracted items.}

Returns a @tech{subprogram} @racketid[P] that extracts files from
@racket[in] with respect to @racket[(current-directory)].

If @racket[in] is a @racket[path-string?], then @racket[(extract in)]
is equivalent to @racket[(call-with-input-file in extract)]. The rest
of this entry assumes @racket[in] is an input port.

If @racketid[P] cannot figure out how to extract files, it will return
@racket[FAILURE] and log @racket[($extract-report 'unsupported
(object-name in))].

If @racketid[P] succeeds, it will return @racket[(void)] and log
@racket[($extract-report 'done (object-name in))].

The extraction process infers the archive format from the file
extension in @racket[(object-name in)].  If this is @racket{.tar},
then @racketid[P] behaves like @racket[untar].  If @racket{.tgz} or
@racket{.tar.gz}, then @racket[untgz]. If @racket{.zip},
@racket[unzip]. Otherwise, @racketid[P] will attempt to use
@racket[current-find-extract-procedure].
}


@defproc[(extract-input [name string?] [#:keep? any/c #f]) subprogram?]{
Like @racket[extract], except archive data is drawn from the input
found using @racket[(input-ref name)] and @racket[resolve-input].  If
@racket[keep?] is @racket[#f], the input is released using
@racket[release-input] after extraction.
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
