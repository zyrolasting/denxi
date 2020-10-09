#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    xiden/logged
                    xiden/port
                    xiden/rc
                    xiden/source
                    xiden/url]
                    "../shared.rkt"]

@title{Data Sourcing}

@defmodule[xiden/source]

A @deftech{source} is a Racket string that refers to bytes.  The
string may contain a URL, a filesystem path, or custom information
understood by a plugin.

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



@require["../shared.rkt"
         @for-label[racket/base
                    racket/contract
                    xiden/cmdline
                    xiden/message
                    xiden/printer]]

@section{Transferring Bytes}

@defmodule[xiden/port]

@racketmodname[xiden/port] reprovides all bindings from
@racketmodname[racket/port], in addition to the bindings defined in
this section.

@defproc[(mebibytes->bytes [mebibytes real?]) exact-nonnegative-integer?]{
Converts mebibytes to bytes, rounded up to the nearest exact integer.
}

@defproc[(transfer [bytes-source input-port?]
                   [bytes-sink output-port?]
                   [#:on-status on-status (-> $transfer? any)]
                   [#:max-size max-size (or/c +inf.0 exact-positive-integer?)]
                   [#:buffer-size buffer-size exact-positive-integer?]
                   [#:transfer-name transfer-name non-empty-string?]
                   [#:est-size est-size (or/c +inf.0 real?)]
                   [#:timeout-ms timeout-ms (>=/c 0)])
                   void?]{
Like @racket[copy-port], except bytes are copied from
@racket[bytes-source] to @racket[bytes-sink], with at most
@racket[buffer-size] bytes at a time.

@racket[transfer] applies @racket[on-status] repeatedly and
synchronously with @racket[$transfer] @tech{messages}.  Each such
message is tagged with the given @racket[transfer-name] to identify
the specific transfer.

@racket[transfer] sets a budget @racketid[N] for the maximum expected
size using @racket[est-size] and @racket[max-size]. @racket[max-size]
is the hard upper limit for total bytes to copy (typically decided by
the user). If @racket[max-size] is @racket[+inf.0], then
@racket[transfer] will not terminate if @racket[bytes-source] does not
produce @racket[eof]. @racket[est-size] is an estimate for the number
of bytes that @racket[bytes-source] will produce (typically
@italic{not} decided by the user) . If @racket[(> est-size max-size)],
then the transfer will not start to respect the user's budget.
Otherwise @racketid[N] is bound to @racket[est-size] to hold
@racket[bytes-source] accountable for the estimate.

@racket[transfer] will copy no more than @racketid[N] bytes, and will
wait no longer than @racket[timeout-ms] for the next available byte.
}


@defstruct*[($transfer $message) ([name string?]) #:prefab]{
Represents a transfer status with a given @racket[name]. The name is
derived from a @tech{package input}'s name, or the name of a source
used for that input.
}

@defstruct*[($transfer-progress $transfer) ([bytes-read exact-nonnegative-integer?] [max-size (or/c +inf.0 exact-positive-integer?)] [timestamp exact-positive-integer?]) #:prefab]{
Represents progress transferring bytes to a local source.

Unless @racket[max-size] is @racket[+inf.0], @racket[(/ bytes-read
max-size)] approaches @racket[1].  You can use this along with the
@racket[timestamp] (in seconds) to reactively compute an estimated
time to complete.
}


@defstruct*[($transfer-small-budget $transfer) () #:prefab]{
A request to transfer bytes was rejected because the user does not
allow downloads of a required size.  This message also applies if a
transfer cannot estimate the number bytes to read, and the user does
not allow unlimited transfers.

See @racket[XIDEN_FETCH_TOTAL_SIZE_MB] and @racket[XIDEN_FETCH_PKGDEF_SIZE_MB].
}


@defstruct*[($transfer-over-budget $message) ([size exact-positive-integer?]) #:prefab]{
A request to transfer bytes was halted because the transfer read more
than @racket[size] bytes, which the user's configuration forbids.

See @racket[XIDEN_FETCH_TOTAL_SIZE_MB] and @racket[XIDEN_FETCH_PKGDEF_SIZE_MB].
}


@defstruct*[($transfer-timeout $message) ([bytes-read exact-nonnegative-integer?]) #:prefab]{
A request to transfer bytes was halted after @racket[bytes-read] bytes
because no more bytes were available after so much time.

See @racket[XIDEN_FETCH_TIMEOUT_MS].
}
