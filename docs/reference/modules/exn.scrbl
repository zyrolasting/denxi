#lang scribble/manual

@require[@for-label[xiden/exn xiden/l10n] "../../shared.rkt"]

@title{Exceptions}

@defmodule[xiden/exn]

racketmodname[xiden/exn] defines exceptions for control in subprograms
that are not @tech{message}-oriented.  It also provides procedures for
constructing and raising exceptions.  These procedures are not
documented because they are private, and simple enough to be
self-evident for maintainers.

@deftogether[(
@defstruct*[(exn:fail:xiden exn:fail) ()]
@defstruct*[(exn:fail:user:xiden exn:fail:user) ()]
)]{
Project-specific exception types.

For some instance @racket[E] of these exception types, it may be the
case that @racket[(equal? (exn-message E) "")]. This is because some
exceptions are meant for end-user display, and they should first be
converted to @tech{messages} for use with
@racket[(get-message-formatter)].

}
