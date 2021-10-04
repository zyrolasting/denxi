#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/fasl
                    racket/match
                    racket/pretty
                    racket/serialize
                    denxi/l10n
                    denxi/subprogram
                    denxi/format
                    denxi/message
                    denxi/port
                    denxi/printer]
         @for-syntax[denxi/printer]
         denxi/printer
         "../../shared.rkt"]

@title{Printer}

@defmodule[denxi/printer]

@racketmodname[denxi/printer] writes @tech{messages} as bytes on
output ports.


@defstruct*[($verbose $message) ([message $message?]) #:prefab]{
A wrapper for a message that only appears to a user if
@racket[(DENXI_VERBOSE)] is @racket[#t].
}


@defsetting*[DENXI_FASL_OUTPUT]{
When true, each value @racket[v] printed on STDOUT is first transformed using
@racket[(s-exp->fasl (serialize v))].
}

@defsetting*[DENXI_READER_FRIENDLY_OUTPUT]{
When true, each program output value @racket[v] is printed on STDOUT using
@racket[pretty-write] without being translated to a human-readable message.

Use this to produce @racket[(read)]able logs. If it aids read performance,
combine with @racket[DENXI_FASL_OUTPUT].
}

@defsetting*[DENXI_VERBOSE]{
When true, emit more detailed program output.
}

@defproc[(write-message-log [messages (or/c $message? subprogram-log/c)] [format-message message-formatter/c]) void?]{
Writes every message @racket[m] in @racket[messages] using
@racket[(write-message m format-message (current-output-port))].
}


@defproc[(write-message [m $message?] [#:newline? newline? any/c #t] [format-message message-formatter/c] [out output-port? (current-output-port)]) void?]{
Writes a @tech{message} to @racket[out] according to the values of
@racket[(DENXI_READER_FRIENDLY_OUTPUT)], @racket[(DENXI_FASL_OUTPUT)],
and @racket[(DENXI_VERBOSE)].

Given @racket[(and (not (DENXI_VERBOSE)) ($verbose? m))],
@racket[write-message] does nothing.

Otherwise, @racket[write-message] does the following:

@racketblock[
(let ([to-send (if (DENXI_READER_FRIENDLY_OUTPUT) m (format-message m))])
  (if (DENXI_FASL_OUTPUT)
      (s-exp->fasl (serialize to-send) out)
      (if (DENXI_READER_FRIENDLY_OUTPUT)
          (pretty-write #:newline? newline? to-send out)
          ((if newline? displayln display) to-send out))))]

Note that @racket[newline?] is ignored when using fasl output.
}
