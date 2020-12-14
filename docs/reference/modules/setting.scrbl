#lang scribble/manual

@(require (for-label racket/base racket/contract xiden/rc xiden/setting)
          (for-syntax "../../shared.rkt"
                      racket
                      syntax/stx
                      xiden/rc
                      xiden/setting
                      xiden/cli-flag)
          xiden/rc
          xiden/setting
          xiden/cli-flag
          "../../shared.rkt")

@title{Settings}

@defmodule[xiden/setting]

A @deftech{setting} is an instance of the @racket[setting]
@tech/reference{structure}.  Settings are used as a canonical source
of dynamically bound values, along with validation information and
contextual help.

@defstruct*[setting ([id symbol?] [valid? predicate/c] [parameter parameter?] [derived-parameter parameter?])]{
Defines a @tech{setting}. You likely do not need to create an instance
directly because the constructor does not enforce a meaningful
structural relationship between the fields. Use
@racket[define-setting] instead.

@racket[setting] implements @racket[prop:procedure]. For an instance @racket[S]:

@itemlist[
@item{@racket[(S)] is @racket[((setting-derived-parameter S))].}
@item{@racket[(S val proc)] applies the procedure @racket[proc] in a @tech/reference{parameterization} where @racket[(setting-derived-parameter S)] is @racket[val].}
]
}


@defform[(define-setting id contract-expr default-expr)]{
Binds a new @tech{setting} to @racket[id].

@racket[contract-expr] must evaluate to a @tech/reference{flat contract}.
Any attempt to install a value in the setting that does not pass this
contract will fail.

@racket[default-expr] must evaluate to either a @racket[(-> symbol?
any/c)] procedure, or a non-procedure. The procedure form must accept
@racket[id] (as a symbol) as the sole formal argument and return a
default value.

@racketblock[
(define-setting PICKED_NUMBER (integer-in 0 100))
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
