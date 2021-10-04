#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/match
                    racket/string
                    denxi/format
                    denxi/message]
                    "../../shared.rkt"]


@title{Formatting}

@defmodule[denxi/format]

@racketmodname[denxi/format] provides all bindings from
@racketmodname[racket/format], including the below bindings.

@section{Conventional Formatting Procedures}

@defproc[(format-symbol-for-display [sym symbol?]) string?]{
Like @racket[symbol->string], except the resulting string wraps the
symbol's string content in conventional quotes for display in
human-readable output.
}

@defproc[(indent-lines [lines (listof string?)]) (listof string?)]{
Returns a new version of @racket[lines] where each element has two
leading spaces.
}

@defproc[(join-lines [#:trailing? trailing? any/c #f]
                     [#:suffix suffix (or/c char? string? #f)]
                     [lines (listof string?)]) string?]{
Combines @racket[lines] into a single string.

If @racket[suffix] is @racket[#f], then @racket[join-lines] will use a
platform-specific suffix.

If @racket[trailing?] is @racket[#t], then the last line will also end
in the suffix.
}

@defproc[(join-lines* [#:trailing? trailing? any/c #f]
                      [#:suffix suffix (or/c char? string? #f)]
                      [line string?] ...)
                      string?]{
Like @racket[join-lines], except lines are accumulated from formal
arguments.
}


@section{Message Formatting}

@tech{Messages} are useful for exchanging information without
formatting concerns, but program output must include human-readable
strings. This section therefore defines a @deftech{message formatter}
as a procedure that translates a @tech{message} to a string. The
bindings documented in this section pertain exclusively to message
formatters.

@defthing[message-formatter/c chaperone-contract? #:value (-> $message? string?)]{
A contract that matches a @tech{message formatter}.
}

@defform[(message-formatter patts ...)]{
Expands to @racket[(λ (m) (match m patts ...))].

@racketblock[
(define-message $foo (a b c))

(code:comment "\"1 2 3\"")
((message-formatter [($foo x y z) (format "~a ~a ~a" x y z)]) ($foo 1 2 3))
]
}

@defform[(define-message-formatter id patts ...)]{
Expands to @racket[(define id (message-formatter patts ...))]
}

@defform[(define+provide-message-formatter id patts ...)]{
Expands to

@racketblock[
(begin (provide (contract-out [id message-formatter/c]))
       (define-message-formatter id patts ...))]
}

@defproc[(combine-message-formatters [formatter message-formatter/c] ...) message-formatter/c]{
Returns a @tech{message formatter} that uses each @racket[formatter]
in the order passed.
}

@defthing[default-message-formatter message-formatter/c]{
A @tech{message formatter} useful only for producing locale-independent fallback strings.
}

@defthing[current-message-formatter (parameter/c message-formatter/c) #:value default-message-formatter]{
A @tech/reference{parameter} holding the @tech{message formatter} for
use with @racket[format-message].
}

@defproc[(format-message [m $message?]) string?]{
Equivalent to @racket[((current-message-formatter) m)].
}
