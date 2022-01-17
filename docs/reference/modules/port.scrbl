#lang scribble/manual

@require[@for-label[racket
                    denxi/subprogram
                    denxi/format
                    denxi/port
                    denxi/message
                    denxi/printer
                    denxi/source
                    denxi/url]
         "../../shared.rkt"]

@title{Ports}

@(define i (racket in))
@(define o (racket out))

@defmodule[denxi/port]

@racketmodname[denxi/port] reprovides all bindings from
@racketmodname[racket/port], in addition to the bindings defined in
this section.

@defproc[(mebibytes->bytes [mebibytes real?]) exact-nonnegative-integer?]{
Converts mebibytes to bytes, rounded up to the nearest exact integer.
}

@deftogether[(
@defproc[(transfer [in input-port?]
                   [out output-port?]
		   [policy transfer-policy?]
                   void?)]
@defstruct*[transfer-policy ([buffer-size exact-positive-integer?]
                       	     [est-size (or/c +inf.0 real?)]
		       	     [max-size (or/c +inf.0 exact-nonnegative-integer?)]
		       	     [timeout-ms (>=/c 0)]
		       	     [transfer-name string?]
		       	     [telemeter (-> $transfer? void?)])]
)]{
Like @racket[(copy-port in out)], with safety limits defined by an
instance of @racket[transfer-policy]. All behavior is synchronous.

@racket[buffer-size] controls the maximum number of bytes that may
drawn from @i at a time, in addition to Racket's internal buffering.

@racket[timeout-ms] is the number of milliseconds to wait for the next
available byte from @|i|.

@racket[telemeter] is an effectual procedure for capturing the status
of a transfer.

@racket[est-size] and @racket[max-size] control the maximum number of
bytes to read from @|i|. @racket[est-size] is an untrusted estimate of
the number of bytes @|i| will produce. @racket[max-size] is a trusted
statement of the maximum tolerable number of bytes.

When @racket[(> est-size max-size)] @racket[transfer] reports
@racket[$transfer:budget:rejected] and ends.

When @racket[(<= est-size max-size)], @racket[transfer] will read no
more than @racket[est-size] bytes.

If both @racket[est-size] and @racket[max-size] are @racket[+inf.0],
then @racket[transfer] will not terminate if @|i| does not
end.
}

@defthing[zero-trust-transfer-policy transfer-policy/c]{
@racket[(transfer in out zero-trust-transfer-policy)] has no effect,
for all values of @|i| and @|o|.
}

@defstruct*[($transfer $message) () #:prefab]{
A @tech{message} pertaining to a @racket[transfer] status.
}

@defthing[transfer-policy/c contract?]{
A @tech/reference{contract} for @racket[transfer-policy] instances.
}

@defstruct*[($transfer:scope $transfer) ([name string?]
                                         [timestamp-s exact-positive-integer?]
                                         [message (and/c $transfer? (not/c $transfer:scope?))]) #:prefab]{
Contains a @racket[$transfer] message from a call to @racket[transfer]
where the @racketid[transfer-name] argument was bound to
@racket[name]. @racket[timestamp-s] is a value in the range of
@racket[current-seconds].
}

@defstruct*[($transfer:broken $transfer) ([value any/c]) #:prefab]{
Represents an unexpected synchronization result from
@racket[read-bytes-avail!-evt]. Encountering this value is a sign that
invariants are failing in the Racket runtime.
}

@defstruct*[($transfer:progress $transfer) ([bytes-read exact-nonnegative-integer?]
                                            [max-size (or/c +inf.0 exact-positive-integer?)]) #:prefab]{
Represents progress transferring bytes to a local source.

Unless @racket[max-size] is @racket[+inf.0], @racket[(/ bytes-read
max-size)] approaches @racket[1].
}


@defstruct*[($transfer:budget $transfer) () #:prefab]{
A message pertaining to a transfer space budget.
}


@defstruct*[($transfer:budget:exceeded $message) ([size exact-positive-integer?]) #:prefab]{
A request to transfer bytes was halted because the transfer read
@racket[overrun-size] bytes more than @racket[allowed-max-size] bytes.

See @racket[DENXI_FETCH_TOTAL_SIZE_MB] and @racket[DENXI_FETCH_PKGDEF_SIZE_MB].
}


@defstruct*[($transfer:budget:rejected $message) ([proposed-max-size (or/c +inf.0 exact-positive-integer?)]
                                                  [allowed-max-size exact-positive-integer?]) #:prefab]{
A request to transfer bytes never started because the transfer estimated
@racket[proposed-max-size] bytes, which exceeds the user's maximum of @racket[allowed-max-size].

See @racket[DENXI_FETCH_TOTAL_SIZE_MB] and @racket[DENXI_FETCH_PKGDEF_SIZE_MB].
}



@defstruct*[($transfer:timeout $message) ([bytes-read exact-nonnegative-integer?] [wait-time (>=/c 0)]) #:prefab]{
A request to transfer bytes was halted after @racket[bytes-read] bytes
because no more bytes were available after @racket[wait-time]
milliseconds.

See @racket[DENXI_FETCH_TIMEOUT_MS].
}
