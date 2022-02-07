#lang denxi/document

@title{Sources}

@defmodule[denxi/source]

A @deftech{source} is a value that implements @racket[gen:source].
When used with @racket[fetch], a source produces an input port and an
estimate of how many bytes that port can produce. Denxi uses sources
to read data with safety limits. To @deftech{tap} a source means
gaining a reference to the input port and estimate. To
@deftech{exhaust} a source means gaining a reference to a contextual
error value. We can also say a source is @deftech{tapped} or
@deftech{exhausted}.

Note that these terms are linguistic conveniences. There is no value
representing a tapped or exhausted state. The only difference is where
control ends up in the program, and what references become available
as a result of using @racket[fetch] on a source.


@defthing[budget/c flat-contract? #:value (or/c +inf.0 exact-nonnegative-integer?)]{
A value representing an estimated number of bytes to allocate for a
future task.
}

@defthing[tap/c chaperone-contract? #:value (-> input-port? budget/c any/c)]{
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
@defproc[(identify [source source?]) input-port?]
)]{
@racket[gen:source] is a @tech/reference{generic interface} that
requires an implementation of @racket[fetch] and
@racket[identify]. @racket[source?] returns @racket[#t] for values
that at least partially implement @racket[gen:source].

@racket[fetch] attempts to @tech{tap} @racket[source]. If successful,
@racket[fetch] calls @racket[tap] in tail position, passing the input
port and the estimated maximum number of bytes that port is expected
to produce. Otherwise, @racket[fetch] calls @racket[exhaust] in tail
position using a source-dependent argument.

@racket[identify] returns an input port. The bytes read from the port
identify a particular instance of some value that implements
@racket[gen:source].
}

@defthing[source-variant? predicate/c]{
Returns @racket[#t] if the sole argument is suitable for use in @racket[coerce-source].
}

@deftogether[(
@defproc[(subprogram-fetch [source source?] [tap tap/c]) subprogram?]
@defstruct*[($fetch $message) ([id bytes?] [errors (listof $message?)])]
)]{
Returns a @tech{subprogram} that applies @racket[fetch] to
@racket[source] and @racket[tap].

The computed value of the subprogram is @racket[FAILURE] if the
source is @tech{exhausted}. Otherwise, the value is what's returned
from @racket[tap].

The log will gain a @racket[($fetch id errors)] message, where
@racket[id] is @racket[(make-source-key source)], and
@racketid[errors] is empty if the fetch is successful.
}

@defproc[(make-source-key [src source?]) bytes?]{
Returns a message digest computed in terms of @racket[(identify src)].
If the source is not meant to be identifiable, then the byte string
does not have to be unique.
}

@defthing[current-string->source (parameter/c (-> string? source?))]{
A parameter that controls how @racket[coerce-source] converts strings
to @tech{source} values.

The default value will infer if the string is suitable for use with
@racket[file-source] or @racket[http-source], in that order. If an
error is raised, it will be returned within an @racket[exhausted-source].
}


@defproc[(make-limited-tap [max-size exact-nonnegative-integer?]) tap/c]{
Returns a procedure suitable for use in @racket[fetch], such that the
input port produced from tapping a source will never exceed
@racket[max-size].
}



@section{Defining Source Types}

@defform[(define-source #:key compute-key
           (id [field field-contract] ...)
             body ...)]{
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

@defproc[(lock-source [variant source-variant?]
                      [budget (or/c +inf.0 exact-nonnegative-integer?) 0]
                      [exhaust exhaust/c])
                      (or/c source-variant?
                            bytes?)]{
Returns either @racket[variant], or the complete byte string produced
from @racket[variant] so long as its estimated size is not greater
than @racket[budget].

@racket[lock-source] works in the context of @racket[fetch], using the
provided @racket[exhaust] procedure.
}


@section{Source Types}

@defstruct*[exhausted-source ([value any/c])]{
A source that is always @tech{exhausted}, meaning that
@racket[(fetch (exhausted-source v) void values)] returns @racket[v].

@racket[(identify exhausted-source)] is an input port that produces
only @racket[eof].
}


@defstruct*[byte-source ([data bytes?])]{
A source that, when @tech{tapped}, yields bytes directly from
@racket[data].
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
A @tech{source} that, when @tech{tapped}, produces bytes from the
given string.
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


@defstruct[http-source ([request-url (or/c url? url-string?)] [max-redirects exact-nonnegative-integer?])]{
A @tech{source} that, when @tech{tapped}, yields bytes from an HTTP
response body. The response comes from a GET request to
@racket[request-url], and the body is only used for a 2xx response.  A
non-2xx status will @tech{exhaust} the source with an
@racket[$http-failure] message. The internal HTTP client will follow
up to @racket[max-redirects] redirections.

If @racket[request-url] has the @racket{file} scheme, then
@racket[http-source] behaves like @racket[file-source]. In this case,
only the URL path is used from @racket[request-url].

If the source is @tech{exhausted}, it yields a relevant exception.
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

The following procedures are useful for declaring @tech{sources} in a
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
