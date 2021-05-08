#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    xiden/cmdline
                    xiden/subprogram
                    xiden/format
                    xiden/port
                    xiden/source
                    xiden/message
                    @except-in[xiden/pkgdef #%module-begin url]
                    xiden/printer
                    xiden/url]
        @for-syntax[xiden/source]
                    xiden/source
                    "../../shared.rkt"]


@title{Sources}

@defmodule[xiden/source]

A @deftech{source} is a value that implements @racket[gen:source].
When used with @racket[fetch], a source produces an input port and an
estimate of how many bytes that port can produce. Xiden uses sources
to read data with safety limits. To @deftech{tap} a source means
gaining a reference to the input port and estimate. To
@deftech{exhaust} a source means gaining a reference to a contextual
error value. We can also say a source is @deftech{tapped} or
@deftech{exhausted}.

Note that these terms are linguistic conveniences. There is no value
representing a tapped or exhausted state. The only difference is where
control ends up in the program, and what references become available
as a result of using @racket[fetch] on a source.


@defthing[tap/c
  chaperone-contract?
  #:value
  (-> input-port?
      (or/c +inf.0 exact-positive-integer?)
      any/c)]{
A @tech/reference{contract} for a procedure used to @tech{tap} a
@tech{source}.

The procedure is given an input port, and an estimate of the maximum
number of bytes the port can produce. This estimate could be
@racket[+inf.0] to allow unlimited reading, provided the user allows
this in their configuration.
}

@defthing[exhaust/c chaperone-contract? #:value (-> any/c any/c)]{
A @tech/reference{contract} for a procedure used when a source is
@tech{exhausted}.

The sole argument to the procedure depends on the source type.
}

@deftogether[(
@defidform[gen:source]
@defthing[source? predicate/c]
@defproc[(fetch [source source?] [tap tap/c] [exhaust exhaust/c]) any/c]
@defproc[(identify [source source?]) (or/c input-port? #f)]
)]{
@racket[gen:source] is a @tech/reference{generic interface} that
requires an implementation of @racket[fetch] and
@racket[identify]. @racket[source?] returns @racket[#t] for values
that do so.

@racket[fetch] attempts to @tech{tap} @racket[source]. If successful,
@racket[fetch] calls @racket[tap] in tail position, passing the input
port and the estimated maximum number of bytes that port is expected
to produce. Otherwise, @racket[fetch] calls @racket[exhaust] in tail
position using a source-dependent argument.

@racket[identify] attempts to return an input port that produces bytes
used to identify a @racket[source?] value. The input port does not
produce bytes for expected content, and cannot use any information
outside of what's available in the value itself. This allows other
other systems to compute cache keys and avoid unnecessary calls to
@racket[fetch]. If @racket[identify] returns @racket[#f] for a source,
then that source cannot be identified.
}

@defthing[source-variant? predicate/c]{
Returns @racket[#t] if the sole argument is suitable for use in @racket[coerce-source].
}

@deftogether[(
@defproc[(subprogram-fetch [id any/c] [source source?] [tap tap/c]) subprogram?]
@defstruct*[($fetch $message) ([id any/c] [errors (listof $message?)])]
)]{
Returns a @tech{subprogram} that applies @racket[fetch] to
@racket[source] and @racket[tap].

The computed value of the subprogram is @racket[FAILURE] if the
source is @tech{exhausted}. Otherwise, the value is what's returned
from @racket[tap].

The log will gain a @racket[($fetch id errors)] message, where
@racketid[errors] is empty if the fetch is successful.
}

@defproc[(make-source-key [src source?]) (or/c #f bytes?)]{
Returns bytes to uniquely identify @racket[src], or @racket[#f]
if the source is not meant to be identified.

This is a front-end to @racket[identify] that will always produce a
fixed-length byte string. Prefer using this over using
@racket[identify] directly.
}

@defthing[current-string->source (parameter/c (-> string? source?))]{
A parameter that controls how @racket[coerce-source] converts strings
to @tech{source} values.

The default value will infer if the string is suitable for use with
@racket[file-source] or @racket[http-source], in that order. If an
error is raised, it will be returned within an @racket[exhausted-source].
}

@section{Source and Fetch Settings}

@defsetting*[XIDEN_DOWNLOAD_MAX_REDIRECTS]{
The maximum number of HTTP redirects to follow when resolving a GET request.
}

@defsetting*[XIDEN_FETCH_TOTAL_SIZE_MB]{
The maximum total size of a single download allowed when fetching an input from
a source, in mebibytes.
}

@defsetting*[XIDEN_FETCH_BUFFER_SIZE_MB]{
The maximum number of bytes to read at a time from a source, in mebibytes.
}

@defsetting*[XIDEN_FETCH_PKGDEF_SIZE_MB]{
Like @racket[XIDEN_FETCH_TOTAL_SIZE_MB], except the quota only applies
to @tech{package definitions} named in a user-defined transaction.
This quote does not apply to @tech{package definitions} listed
as inputs in another @tech{package definition}.
}

@defsetting*[XIDEN_FETCH_TIMEOUT_MS]{
The maximum number of seconds to wait for the next available byte from a
source.
}


@section{Defining Source Types}

@defform[(define-source #:key compute-key (id [field field-contract] ...) body ...)]{
Defines a new @tech{source} type.

On expansion, @racket[define-source] defines a new structure type
using @racket[(struct id (field ...))]. The type is created with a
guard that enforces per-field contracts. Instances implement
@racket[gen:source].

@racket[define-source] injects several bindings into the lexical
context of @racket[body]:

@itemlist[
@item{
@racket[%src], @racket[%tap], and @racket[%fail] are each bound to
their respective formal argument of @racket[fetch].
}

@item{
@racket[%fetch] is @racket[(bind-recursive-fetch %tap %fail)].
}

@item{
Each @racket[field] identifier is bound to a respective value
for an instance of the structure type.
}
]

To understand how these injected bindings work together, let's go
through a few examples.

Use @racket[%tap] to fulfil data with an input port and an estimated
data length. In the simplest case, you can return constant data.

@racket[byte-source] uses @racket[%tap] like so:

@racketblock[
(define-source #:key byte-source-data (byte-source [data bytes?])
  (%tap (open-input-bytes data)
        (bytes-length data)))]

Notice that the @racket[data] is used to both define a @racket[data]
field (where it appears by @racket[bytes?]) and to reference the value
contained in that field (within @racket[open-input-bytes] and
@racket[bytes-length]).

Use @racket[%fail] in tail position with error information to
indicate a source was @tech{exhausted}.

@racket[file-source] uses @racket[%fail] like so:

@racketblock[
(define-source #:key file-source-path (file-source [path path-string?])
  (with-handlers ([exn:fail:filesystem? %fail])
    (%tap (open-input-file path)
          (+ (* 20 1024) ; Pad out to factor in Mac OS resource forks
             (file-size path)))))
]

Note that @racket[%fail] is an @racket[exhaust/c] procedure, so it
does not have to be given an exception as an argument.

@racket[%fetch] is a recursive variant of @racket[fetch] that uses
@racket[%tap], but a possibly different @racket[exhaust/c] procedure.
This allows sources to control an entire fetch process and fall back
to alternatives.

@racket[first-available-source] uses a resursive fetch to iterate on
available sources until it has none left to check.

@racketblock[
(define-source #:key first-available-source-sources
               (first-available-source [available (listof source?)] [errors list?])
  (if (null? available)
      (%fail (reverse errors))
      (%fetch (car available)
              (λ (e)
                (%fetch (first-available-source (cdr available) (cons e errors))
                        %fail)))))
]

Finally, @racket[%src] is just a reference to an instance of the
structure containing each field.

@racket[compute-key] is an expression that must resolve to a
procedure, or @racket[#f]. It is used to generate an implementation of
@racket[identify], such that the procedure must produce a value that
can be reasonably coerced to an input port for use with
@racket[identify]. Currently, the accepted values include input ports,
paths, strings, byte strings, instances of @racket[url], @tech{sources}, or
lists of the aforementioned types.
}


@defproc[(bind-recursive-fetch [%tap tap/c] [%fail exhaust/c]) (->* (source?) (exhaust/c) any/c)]{
Returns a @racket[fetch]-like procedure that accepts only a source and
an optional procedure to mark the source exhausted (defaults to
@racket[%fail]).
}


@section{Source Types}

@defstruct*[exhausted-source ([value any/c])]{
A source that is always @tech{exhausted}, meaning that
@racket[(fetch (exhausted-source v) void values)] returns @racket[v].
}


@defstruct*[byte-source ([data bytes?])]{
A source that, when @tech{tapped}, yields
bytes directly from @racket[data].
}

@defstruct*[first-available-source ([sources (listof sources?)] [errors list?])]{
A @tech{source} that, when @tech{tapped}, yields bytes from the first
tapped source.

If all sources for an instance are exhausted, then the instance is
exhausted.  As sources are visited, errors are functionally
accumulated in @racket[errors].

The value produced for an exhausted @racket[first-available-source] is
the longest possible list bound to @racket[errors].
}


@defstruct[text-source ([data string?])]{
A @tech{source} that, when @tech{tapped}, produces bytes from the given string.
}


@defstruct[lines-source ([suffix (or/c #f char? string?)] [lines (listof string?)])]{
A @tech{source} that, when @tech{tapped}, produces bytes from
@racket[(join-lines #:suffix suffix #:trailing? #f lines)].

@racketblock[
(define src
  (lines-source "\r\n"
                '("#lang racket/base"
                  "(provide a)"
                  "(define a 1)")))

(code:comment "\"#lang racket/base\r\n(provide a)\r\n(define a 1)\r\n\"")
(fetch src consume void)
]
}


@defstruct[file-source ([path path-string?])]{
A @tech{source} that, when @tech{tapped}, yields bytes
from the file located at @racket[path].

If the source is @tech{exhausted}, it yields a relevant
@racket[exn:fail:filesystem] exception.
}


@defstruct[http-source ([request-url (or/c url? url-string?)])]{
A @tech{source} that, when @tech{tapped}, yields bytes from an HTTP
response body. The response comes from a GET request to
@racket[request-url], and the body is only used for a 2xx response.  A
non-2xx status will @tech{exhaust} the source with an
@racket[$http-failure] message.

If @racket[request-url] has the @racket{file} scheme, then
@racket[http-source] behaves like @racket[file-source]. In this case,
only the URL path is used from @racket[request-url].

If the source is @tech{exhausted}, it yields a relevant exception.

The behavior of the source is impacted by @racket[XIDEN_DOWNLOAD_MAX_REDIRECTS].
}


@defstruct[http-mirrors-source ([request-urls (listof (or/c url-string? url?))])]{
Like @racket[http-source], but tries each of the given URLs using
@racket[first-available-source].
}

@defstruct*[($http-failure $message) ([request-url string?]
                                      [status-line string?]
                                      [headers (listof (cons/c string? string?))]
                                      [capped-body bytes?])]{
Represents a server response when an @racket[http-source] is
@tech{exhausted}. The field values match their names.
@racket[cappped-body] in special in that it holds no more
than 512 bytes of the response body.
}


@section{Source Expressions}

The following procedures are useful for declaring sources in a
@tech{package input}.

@defproc[(sources [variant (or/c string? source?)] ...) source?]{
Like @racket[first-available-source], but each string argument is
coerced to a source using @racket[coerce-source].
}

@defproc[(coerce-source [variant source-variant?]) source?]{
Returns a @tech{source} depending on the type of @racket[variant].

@itemlist[
@item{
If @racket[variant] is a @tech{source}, then the returned value is
@racket[variant].
}

@item{
If @racket[variant] is a string, then the returned value is @racket[((current-string->source) variant)]}

@item{
If @racket[variant] is a byte string, then the returned value is @racket[(byte-source variant)].
}

@item{
If @racket[variant] is a path, then the returned value is @racket[(file-source variant)].
}
]

}


@defform[(from-file relative-path-expr)]{
Expands to a complete path. @racket[relative-path-expr] is a relative path
made complete with regards to the source directory in which this expression
appears.

Due to this behavior, @racket[from-file] will return different results when the
containing source file changes location on disk.
}


@section{Untrusted Source Expressions}

@deftogether[(
@defstruct*[($bad-source-eval $message) ([reason (or/c 'security 'invariant)] [datum any/c])]
@defproc[(eval-untrusted-source-expression [datum any/c] [ns namespace? (current-namespace)]) subprogram?]
)]{
@racket[eval-untrusted-source-expression] returns a @tech{subprogram}
which evaluates @racket[(eval datum ns)] in the context of a
@tech/reference{security guard}. The security guard blocks all file
operations (except @racket['exists]), and all network operations.

If the evaluation produces a @tech{source}, then the result of the
subprogram is that source, and no other @tech{messages} will
appear in the @tech{subprogram log}.

If the evaluation does not produce a @tech{source}, then the result is
@racket[FAILURE] and the subprogram log gains a
@racket[($bad-source-eval 'invariant datum)].

If the evaluation is blocked by the security guard, then the result is
@racket[FAILURE] and the subprogram log gains a
@racket[($bad-source-eval 'security datum)].
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
