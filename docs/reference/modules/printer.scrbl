#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/fasl
                    racket/match
                    racket/pretty
                    racket/serialize
                    xiden/l10n
                    xiden/subprogram
                    xiden/format
                    xiden/message
                    xiden/port
                    xiden/printer]
         @for-syntax[xiden/printer]
         xiden/printer
         "../../shared.rkt"]

@title{Printer}

@defmodule[xiden/printer]

@racketmodname[xiden/printer] writes @tech{messages} as bytes on
output ports.


@defstruct*[($verbose $message) ([message $message?]) #:prefab]{
A wrapper for a message that only appears to a user if
@racket[(XIDEN_VERBOSE)] is @racket[#t].
}


@defsetting*[XIDEN_FASL_OUTPUT]{
When true, each value @racket[v] printed on STDOUT is first transformed using
@racket[(s-exp->fasl (serialize v))].
}

@defsetting*[XIDEN_READER_FRIENDLY_OUTPUT]{
When true, each program output value @racket[v] is printed on STDOUT using
@racket[pretty-write] without being translated to a human-readable message.

Use this to produce @racket[(read)]able logs. If it aids read performance,
combine with @racket[XIDEN_FASL_OUTPUT].
}

@defsetting*[XIDEN_VERBOSE]{
When true, emit more detailed program output.
}

@defproc[(write-message-log [messages subprogram-log/c] [format-message message-formatter/c]) void?]{
Writes every message @racket[m] in @racket[messages] using
@racket[(write-message m format-message (current-output-port))].
}


@defproc[(write-message [m $message?] [#:newline? newline? any/c #t] [format-message message-formatter/c] [out output-port? (current-output-port)]) void?]{
Writes a @tech{message} to @racket[out] according to the values of
@racket[(XIDEN_READER_FRIENDLY_OUTPUT)], @racket[(XIDEN_FASL_OUTPUT)],
and @racket[(XIDEN_VERBOSE)].

Given @racket[(and (not (XIDEN_VERBOSE)) ($verbose? m))],
@racket[write-message] does nothing.

Otherwise, @racket[write-message] does the following:

@racketblock[
(let ([to-send (if (XIDEN_READER_FRIENDLY_OUTPUT) m (format-message m))])
  (if (XIDEN_FASL_OUTPUT)
      (s-exp->fasl (serialize to-send) out)
      (if (XIDEN_READER_FRIENDLY_OUTPUT)
          (pretty-write #:newline? newline? to-send out)
          ((if newline? displayln display) to-send out))))]

Note that @racket[newline?] is ignored when using fasl output.
}
