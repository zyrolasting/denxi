#lang scribble/manual

@(require (for-label racket/base racket/contract xiden/setting)
          (for-syntax "../../shared.rkt"
                      racket
                      syntax/stx
                      xiden/setting
                      xiden/cli-flag)
          xiden/setting
          xiden/cli-flag
          "../../shared.rkt")

@title[#:tag "settings"]{Settings}

@defmodule[xiden/setting]

A @deftech{setting} is an instance of the @racket[setting]
@tech/reference{structure} type.  Settings contain
@tech/reference{parameters} to allow dynamic configuration.

Settings add canonical names, validation information, and contextual
help to the parameters. Every setting must be configurable using every
approach in @secref{setting-methods}.

Xiden dynamically binds values to settings when launched. A
@deftech{runtime configuration} is a Racket
@tech/reference{parameterization} in effect after Xiden finishes
processing all @secref{setting-methods}. This implies that a runtime
configuration accounts for @italic{all} Racket parameters
(e.g. @racket[current-directory]), and not just the ones defined by
Xiden.


@section[#:tag "setting-methods"]{Methods for Changing Settings}

Here are some ways to change a setting. Each method overrides the
method before it.

@itemlist[
@item{Do nothing. Every setting has a hard-coded default.}
@item{Set an environment variable, e.g. @litchar{export XIDEN_VERBOSE="#t"}. This changes the default value of the named setting.}
@item{Override the value in a @tech{launcher}}
@item{Use a command line flag like @litchar{--XIDEN_VERBOSE '#t'}.}
]


@section{Settings API}

@defstruct*[setting ([id symbol?] [valid? predicate/c] [parameter parameter?] [derived-parameter parameter?])]{
Defines a @tech{setting}. You likely do not need to create an instance
directly because the constructor does not enforce a meaningful
structural relationship between the fields. Use
@racket[define-setting] instead.

@racket[id] is used as the canonical name of the setting. The setting
is understood to accessible directly in @tech{launchers}, as an
environment variable with the name @litchar{<id>}, and as a command
line argument with the form @litchar{--<id>}.

@racket[valid?] must return @racket[#t] for Racket values that may be
bound using @racket[derived-parameter].

@racket[parameter] holds the current value of the setting with no
protections. @racket[derived-parameter] guards and wraps values
produced by @racket[parameter]. All methods used to change settings go
through @racket[derived-parameter].

@racket[setting] implements @racket[prop:procedure]. For an instance @racket[S]:

@itemlist[
@item{@racket[(S)] is equivalent to @racket[((setting-derived-parameter S))].}
@item{@racket[(S val)] is equivalent to @racket[((setting-derived-parameter S) val)].}
@item{@racket[(S val proc)] applies the procedure @racket[proc] in a @tech/reference{parameterization} where @racket[(setting-derived-parameter S)] is @racket[val].}
]
}


@defform[(define-setting id contract-expr default-expr)]{
Binds a new @tech{setting} to @racket[id].

@racket[contract-expr] must evaluate to a @tech/reference["flat
contract"] so that the setting's @racket[valid?] field is useable as a
predicate.  Any attempt to dynamically bind a value in the setting
that does not pass this contract will fail.

@racket[default-expr] must evaluate to either a @racket[(-> symbol?
any/c)] procedure, or a non-procedure. The procedure form must accept
the @racket[symbol?] form of @racket[id] as the sole formal argument
and return a default value.

@racketblock[
(define-setting PICKED_NUMBER (integer-in 0 100) 0)
]
}

@defproc[(call-with-applied-settings [settings (if/c hash?
                                                     (hash/c setting? any/c)
                                                     (listof (cons/c setting? any/c)))]
                                     [thunk (-> any)])
                                     any]{
Applies @racket[thunk] in a @tech/reference{parameterization} where
each @tech{setting} in @racket[settings] is bound to a new value.

@racketblock[
(define-setting USERNAME string? "")
(define-setting PASSWORD string? "")
(call-with-applied-settings (hasheq USERNAME "insecure" PASSWORD "hunter2") PASSWORD)
(call-with-applied-settings (list (cons USERNAME "insecure") (cons PASSWORD "hunter2")) PASSWORD)
]

}
