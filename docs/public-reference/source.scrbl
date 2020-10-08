#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    xiden/logged
                    xiden/rc
                    xiden/source
                    xiden/url]
                    "../shared.rkt"]

@title{Data Sourcing}

@defmodule[xiden/source]

@defthing[request-transfer/c
  chaperone-contract?
  #:value
  (-> input-port?
      (or/c +inf.0 exact-positive-integer?)
      any/c)]{
A @tech/reference{contract} that matches a specific procedure. The
procedure is expected to read bytes from a port, and then return a
single value.

The procedure is given an estimate of the maximum number of bytes to
read. This estimate could be @racket[+inf.0] to allow unlimited
reading, provided the user allows this in their configuration.
}

@defstruct*[fetch-state ([source string?]
                         [name string?]
                         [result any/c]
                         [request-transfer request-transfer/c])
                        #:transparent]{
Represents the status of a @racket[fetch].
}

@defproc[(fetch [name string?]
                [sources (non-empty-listof string?)]
                [request-transfer request-transfer/c])
                logged?]{
Returns a @tech{logged procedure} that returns a @racket[fetch-state].
Any status messages associated with the fetch operation will appear in
@tech{messages} using @racket[name].

A fetch operation consults the given @racket[sources] in order. A
source string can have any format. @racket[fetch] will use the string
as a filesystem path, as a HTTPS URL, and as a plugin-specific value.

If one of these methods manages to find bytes using the source string,
it will apply @racket[request-transfer] to the input port, along with
the estimated maximum number of bytes that port is expected to
produce. If bytes were transferred successfully, then the process is
finished.

Assuming the returned @racket[fetch-state] is @racketid[F]:

@itemlist[
@item{@racket[(fetch-state-name F)] is @racket[name].}
@item{@racket[(fetch-state-source F)] is the element of @racket[sources] most recently used by @racket[fetch], or @racket[#f] if no source produced bytes. @racket[#f] therefore represents failure here.}
@item{@racket[(fetch-state-result F)] is the value returned from the most recent application of @racket[request-transfer], or @racket[#f] if @racket[(fetch-state-source F)] is @racket[#f].}
@item{@racket[(fetch-state-request-transfer F)] is @racket[request-transfer].}
]

}


@section{Source Expressions}

The following procedures are useful for declaring sources in a
@tech{package input}.

@defproc[(sources [v any/c] ...) list?]{
A semantic alias for @racket[list].
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

@section{Data Sourcing Messages}

@defstruct*[($source-fetched $message) ([source-name string?] [fetch-name string?]) #:prefab]{
Bits were successfully read using @racket[fetch].
}

@defstruct*[($fetch-failure $message) ([name string?]) #:prefab]{
No source used in a @racket[fetch] produced any bits.
}

@defstruct*[($source-method-ruled-out $message) ([source-name string?] [fetch-name string?] [method-name string?] [reason string?]) #:prefab]{
One of the means by which @racket[fetch] finds bits did not work.
}

@defstruct*[($unverified-host $message) ([url string?]) #:prefab]{
GET @racket[url] failed because the host did not pass authentication using HTTPS.

See @racket[XIDEN_TRUST_UNVERIFIED_HOST].
}
