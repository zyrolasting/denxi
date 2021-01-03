#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/fasl
                    racket/match
                    racket/pretty
                    racket/serialize
                    xiden/l10n
                    xiden/logged
                    xiden/format
                    xiden/message
                    xiden/rc
                    xiden/port
                    xiden/printer]
         "../../shared.rkt"]

@title{Printer}

@defmodule[xiden/printer]

@racketmodname[xiden/printer] writes @tech{messages} as bytes on
output ports.


@defstruct*[($verbose $message) ([message $message?]) #:prefab]{
A wrapper for a message that only appears to a user if
@racket[(XIDEN_VERBOSE)] is @racket[#t].
}

@defproc[(write-message-log [messages messy-log/c] [format-message message-formatter/c]) void?]{
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
