#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    xiden/l10n
                    xiden/logged
                    xiden/message
                    xiden/printer
                    xiden/rc]
         "../../shared.rkt"]

@title{Localization}

@defmodule[xiden/l10n]

@(define-syntax-rule (defn sym str ...)
   (item (racket 'sym) ": " str ...))

@racketmodname[xiden/l10n] uses @tech{messages} to communicate with
the user according to the value of @racket[(system-language+country)].
Currently, the only supported locale is @tt{en-US}.


@defproc[(get-message-formatter) message-formatter/c]{
Returns a @tech{message formatter} for translating @tech{messages}
to strings in the user's locale.}


@defproc[(get-localized-string [sym symbol?]) string?]{
Returns a string for the user's locale.

@itemlist[
@defn[top-level-cli-help]{A list of available @litchar{xiden} subcommands}
@defn[show-command-help]{A list of available @litchar{xiden show} subcommands}
@item{@italic{<setting id>}: A short description of the named setting.}
]}


@defproc[(run+print-log [l logged?]) any/c]{
Returns the first value from @racket[(run-log l)].

Before returning control, each @tech{message} @racketid[M] from
@racket[run-log] is printed using

@racketblock[(write-message M (get-message-formatter) (current-output-port))]
}
