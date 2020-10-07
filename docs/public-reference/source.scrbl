#lang scribble/manual

@title{Fetching Bytes from Source}

@defmodule[xiden/source]

@defthing[request-transfer/c
  chaperone-contract?
  #:value
  (-> input-port?
      (or/c +inf.0 exact-positive-integer?)
      any/c)]{

A contract that matches a specific procedure. The procedure is
expected to read bytes from a port, and then return a single value.

The procedure is given an estimate of the maximum number of bytes to
read. This estimate could be @racket[+inf.0] to allow unlimited
reading, provided the user allows this in their configuration.
}

@defstruct*[fetch-state ([source string?] [name string?] [result any/c] [request-transfer request-transfer/c]) #:transparent]{
Represents a Racket value produced from bytes fetched from a source. See @racket[fetch] for more information.
}

@defproc[(fetch [name string?] [sources (non-empty-listof string?)] [request-transfer request-transfer/c]) logged?]{
Returns a @racket[fetch-state]

Reads bytes from the first of the given @racketid[sources]
to produce bytes.

         (if (and (fetch-state? state)
                  (fetch-state-result state))


computation is performed under the given @racketid[name], such that
any error or progress messages appear with that name.


@racket[request-transfer] is a callback that begins reading bytes
from a port. The return value of @racket[request-transfer] is bound
to the @racket[]

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
