#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    denxi/format
                    denxi/l10n
                    denxi/subprogram
                    denxi/message
                    denxi/printer]
         "../../shared.rkt"]

@title{Localization}

@defmodule[denxi/l10n]

@(define-syntax-rule (defn sym str ...)
   (item (racket 'sym) ": " str ...))

@racketmodname[denxi/l10n] uses @tech{messages} to communicate with
the user according to the value of @racket[(system-language+country)].
Currently, the only supported locale is @tt{en-US}.


@defproc[(get-message-formatter) message-formatter/c]{
Returns a @tech{message formatter} for translating @tech{messages}
to strings in the user's locale.}


@defproc[(get-localized-string [sym symbol?]) string?]{
Returns a string for the user's locale.

@itemlist[
@defn[top-level-cli-help]{A list of available @litchar{denxi} subcommands}
@defn[backwards-racket-version-interval]{An error message for when a user expresses a backwards Racket version interval in a syntax class. Mimic this phrasing: “minimum Racket version cannot exceed maximum Racket version”.}
@defn[show-command-help]{A list of available @litchar{denxi show} subcommands}
@item{@italic{<setting id>}: A short description of the named setting.}
]}


@defproc[(run+print-subprogram [l subprogram?]) any/c]{
Returns the first value from @racket[(run-subprogram l)].

Before returning control, each @tech{message} @racketid[M] from
@racket[run-subprogram] is printed using

@racketblock[(write-message M (get-message-formatter) (current-output-port))]
}
