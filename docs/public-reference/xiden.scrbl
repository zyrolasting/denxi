#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    xiden/rc
                    @except-in[xiden/pkgdef #%module-begin]]
         "../shared.rkt"]

@title{Package Definition Language}

This section covers the languages used to express @tech{package
definitions}.

A @deftech{package definition} is a Racket module that combines
discovery information with a rough description of a program.  A
package user or author would declare the inputs, outputs, and
processing steps for that program with the bindings described in this
document.

@section{Reader Extension}

@(defmodulelang* (xiden))

@racketmodname[xiden], as a reader extension, defines a
@racketmodname[xiden/pkgdef] module. Any well-formed
@litchar|{#lang info}| document is a well-formed @litchar|{#lang
xiden}| document.

@tt{raco setup} and @tt{raco pkg} will not read @litchar|{#lang
xiden}| documents.


@section{Module Language}

@defmodule[xiden/pkgdef]

@racketmodname[xiden/pkgdef] is a module language superset
of @racketmodname[setup/infotab].

@defproc[(input [local-name string?]
                [sources (listof string?)]
                [int (or/c #f well-formed-integrity-info/c) #f]
                [sig (or/c #f well-formed-signature-info/c) #f])
          input-info?]{
Declares an input along with optional verification data.
}

@defproc[(install [link-path path-string?] [output-name string?] [pkgdef-source string?]) void?]{
Builds an output named @racket[output-name] in the @tech{workspace}, and
creates a new symbolic link to the output directory at @racket[link-path]
(w.r.t. @racket[(current-directory)]).

The output is defined in regards to the @tech{package definition} created using
@racket[(fetch pkgdef-source)].
}

@defproc[(signature [public-key-variant (or/c bytes? path-string?)] [signature-body (or/c bytes? path-string?)]) well-formed-signature-info/c]{
A contracted @racket[signature-info] constructor.

Declares signature information used to authenticate a responsible party for byte content.
}

@defform[(from-file relative-path-expr)]{
Expands to a complete path. @racket[relative-path-expr] is a relative path
made complete with regards to the source directory in which this expression
appears.

Due to this behavior, @racket[from-file] will return different results when the
containing source file changes location on disk.
}

@defproc[(from-catalogs [query string?] [catalogs (listof string?) (XIDEN_CATALOGS)]) (listof url-string?)]{
Returns a list of URL strings suitable for use in @racket[input]
as possible sources for bytes.

For each string in @racket[catalogs], all occurances of @racket{$QUERY} are
replaced with the @racket[(uri-encode query)] in the output.

Examples:

@racketblock[
(input "my-definition.rkt"
       (from-catalogs "example.com:widget"))
]

@racketblock[
(input "my-definition.rkt"
       (append (from-catalogs "example.com:widget"
                              '("https://mirrorA.example.com/$QUERY"
                                "https://mirrorB.example.com/?q=$QUERY"))
               (from-file "local.rkt")))
]
}
