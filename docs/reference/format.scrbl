#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/match
                    racket/string
                    xiden/format
                    xiden/message]
                    "../shared.rkt"]


@title{Formatting}

@defmodule[xiden/format]

@racketmodname[xiden/format] provides all bindings from
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

@defproc[(join-lines [lines (listof string?)]) string?]{
Equivalent to @racket[(string-join lines "\n")].
}

@defproc[(join-lines* [line string?] ...) string?]{
Equivalent to @racket[(join-lines (list line ...))].
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
