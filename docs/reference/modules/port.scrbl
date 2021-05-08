#lang scribble/manual

@require[@for-label[racket
                    xiden/subprogram
                    xiden/format
                    xiden/port
                    xiden/message
                    xiden/printer
                    xiden/source
                    xiden/url]
         "../../shared.rkt"]

@title{Ports}

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
