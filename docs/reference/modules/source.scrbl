#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    xiden/cmdline
                    xiden/logged
                    xiden/port
                    xiden/rc
                    xiden/source
                    xiden/message
                    xiden/printer
                    xiden/url]
                    "../../shared.rkt"]


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

@defstruct*[fetch-state ([source (or/c #f string?)] [result any/c]) #:transparent]{
Represents the status of a @racket[fetch], such that @racket[source]
is a freeform string that @racket[fetch] uses to compute the value of
@racket[result].

If @racket[source] is @racket[#f], then @racket[result] should be
considered unusable.
}

@defproc[(fetch [name string?]
                [sources (non-empty-listof string?)]
                [request-transfer request-transfer/c])
                logged?]{
Returns a @tech{logged procedure} to compute a @racket[fetch-state].

A fetch operation consults the given @racket[sources] in order. A
source string can have any format. @racket[fetch] will use the string
as a filesystem path, as a HTTPS URL, and as a plugin-specific value.

If one of these methods manages to find bytes using the source string,
it will apply @racket[request-transfer] to the input port, along with
the estimated maximum number of bytes that port is expected to
produce. If bytes were transferred successfully, then the process is
finished.

Any @tech{message} produced by @racket[fetch] is an instance of
@racket[$fetch]. Each instance @racketid[m] is scoped using
@racket[($regarding ($fetch:scope name related-source) m)], where
@racketid[related-source] is an element of @racket[sources], or
@racket[#f]. When @racketid[related-source] is @racket[#f],
@racketid[m] is a source-independent statement about the fetch
operation.

Assuming the returned @racket[fetch-state] is @racketid[F]:

@itemlist[

@item{@racket[(fetch-state-source F)] is the element of
@racket[sources] most recently used by @racket[fetch], or @racket[#f].
If @racket[#f], then no source produced a value.}

@item{@racket[(fetch-state-result F)] is the value returned from the
most recent application of @racket[request-transfer], or @racket[#f]
if @racket[(fetch-state-source F)] is @racket[#f].}

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

@defstruct*[($fetch $message) () #:prefab]{
A @tech{message} pertaining to @racket[fetch].
}

@defstruct*[($fetch:scope $message) ([fetch-name string?] [source-name (or/c string? #f)]) #:prefab]{
Holds a user-provided name that helps trace an instance of
@racket[$fetch] back to a particular application of @racket[fetch].

If @racket[source-name] is not @racket[#f], then the clarified message
pertains to a particular source.
}

@defstruct*[($fetch:fail $message) ([method symbol?] [reason (or/c #f string?)]) #:prefab]{
A particular approach used to compute a value of
@racket[fetch-state-result] failed. The reason for the failure is a
human-readable string bound to @racket[reason].

If @racket[reason] is @racket[#f], then a reason could not be inferred
for the failure.
}

@defstruct*[($fetch:done $fetch) ([ok? boolean?]) #:prefab]{
A call to @racket[fetch] finished. @racket[ok?] is @racket[#t] if the
@racket[fetch-state-result] of the computed @racket[fetch-state] is
not @racket[#f].
}


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
synchronously with @racket[$transfer] @tech{messages}.

@racket[transfer] reads no more than @racketid[N] bytes from
@racket[bytes-source], and will wait no longer than
@racket[timeout-ms] for the next available byte.

The value of @racketid[N] is computed using @racket[est-size] and
@racket[max-size]. @racket[max-size] is the prescribed upper limit for
total bytes to copy. @racket[est-size] is an estimated for the number
of bytes that @racket[bytes-source] will actually produce (this is
typically not decided by the user). If @racket[(> est-size max-size)],
then the transfer will not start.  Otherwise @racketid[N] is bound to
@racket[est-size] to hold @racket[bytes-source] accountable for the
estimate.

If @racket[est-size] and @racket[max-size] are both @racket[+inf.0],
then @racket[transfer] will not terminate if @racket[bytes-source]
does not produce @racket[eof].
}


@defstruct*[($transfer $message) () #:prefab]{
A @tech{message} pertaining to a @racket[transfer] status.
}

@defstruct*[($transfer:scope $transfer) ([name string?] [message (and/c $transfer? (not/c $transfer:scope?))]) #:prefab]{
Contains a @racket[$transfer] message from a call to @racket[transfer]
where the @racketid[transfer-name] argument was bound to
@racket[name].
}

@defstruct*[($transfer:progress $transfer) ([bytes-read exact-nonnegative-integer?]
                                            [max-size (or/c +inf.0 exact-positive-integer?)]
                                            [timestamp exact-positive-integer?]) #:prefab]{
Represents progress transferring bytes to a local source.

Unless @racket[max-size] is @racket[+inf.0], @racket[(/ bytes-read
max-size)] approaches @racket[1].  You can use this along with the
@racket[timestamp] (in seconds) to reactively compute an estimated
time to complete.
}


@defstruct*[($transfer:budget $transfer) () #:prefab]{
A message pertaining to a transfer space budget.
}


@defstruct*[($transfer:budget:exceeded $message) ([size exact-positive-integer?]) #:prefab]{
A request to transfer bytes was halted because the transfer read
@racket[overrun-size] bytes more than @racket[allowed-max-size] bytes.

See @racket[XIDEN_FETCH_TOTAL_SIZE_MB] and @racket[XIDEN_FETCH_PKGDEF_SIZE_MB].
}


@defstruct*[($transfer:budget:rejected $message) ([proposed-max-size (or/c +inf.0 exact-positive-integer?)]
                                                  [allowed-max-size exact-positive-integer?]) #:prefab]{
A request to transfer bytes never started because the transfer estimated
@racket[proposed-max-size] bytes, which exceeds the user's maximum of @racket[allowed-max-size].

See @racket[XIDEN_FETCH_TOTAL_SIZE_MB] and @racket[XIDEN_FETCH_PKGDEF_SIZE_MB].
}



@defstruct*[($transfer:timeout $message) ([bytes-read exact-nonnegative-integer?] [wait-time (>=/c 0)]) #:prefab]{
A request to transfer bytes was halted after @racket[bytes-read] bytes
because no more bytes were available after @racket[wait-time]
milliseconds.

See @racket[XIDEN_FETCH_TIMEOUT_MS].
}
