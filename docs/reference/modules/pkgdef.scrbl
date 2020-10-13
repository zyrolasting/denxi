#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    xiden/logged
                    xiden/package
                    xiden/rc
                    @except-in[xiden/pkgdef #%module-begin]]
         "../../shared.rkt"]

@title{Module Language}

@defmodule[xiden/pkgdef]

@racketmodname[xiden/pkgdef] is a module language superset
of @racketmodname[setup/infotab].


@section{Special Bindings}

@defproc[(install [link-path path-string?] [output-name string?] [pkgdef-source string?]) void?]{
Builds an output named @racket[output-name] in the @tech{workspace}, and
creates a new symbolic link to the output directory at @racket[link-path]
(w.r.t. @racket[(current-directory)]).

The output is defined in regards to the @tech{package definition} created using
@racket[(fetch pkgdef-source)].
}


@section{Additional Bindings}

@itemlist[
@item{@racket[λ]}
@item{@racket[#%app]}
@item{@racket[#%top-interaction]}
@item{@racket[base32]}
@item{@racket[base64]}
@item{@racket[build-path]}
@item{@racket[build-workspace-path]}
@item{@racket[call-with-input]}
@item{@racket[car]}
@item{@racket[case]}
@item{@racket[cdr]}
@item{@racket[current-directory]}
@item{@racket[define-values]}
@item{@racket[error]}
@item{@racket[from-catalogs]}
@item{@racket[from-file]}
@item{@racket[hex]}
@item{@racket[input]}
@item{@racket[input-ref]}
@item{@racket[integrity]}
@item{@racket[keep-input!]}
@item{@racket[lambda]}
@item{@racket[let]}
@item{@racket[let-values]}
@item{@racket[signature]}
@item{@racket[sources]}
@item{@racket[unpack]}
@item{@racket[void]}
]
