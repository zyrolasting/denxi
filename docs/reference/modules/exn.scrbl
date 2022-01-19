#lang scribble/manual

@require[@for-label[denxi/base denxi/exn racket/function]
         "../../shared.rkt"]

@title{Exceptions}

@defmodule[denxi/exn]

@racketmodname[denxi/exn] extends and reprovides
@racketmodname[racket/exn].


@defform[(pitch catch . swing-forms)]{
Expands to an application of @racket[dynamic-pitch], where
@racket[swing-forms] are placed inside a @racket[lambda] form for the
@racket[#:batter] argument. @racket[catch] is used as the value for
@racket[#:catcher]. @racket[#:umpire] is always bound to its default
value.
}

@defproc[(pitch-to-batter #:batter
                          [swing (-> any)]
                          #:umpire
                          [dispute (uncontrained-domain-> any) values]
                          #:catcher
                          [catch (if/c procedure?
                                       (or/c (procedure-arity-includes/c 0)
                                             (procedure-arity-includes/c 1))
                                       any/c)])
                          any]{
Returns values from @racket[dispute] in a @tech{baseball analogy}.

The @deftech{baseball analogy} applies @litchar{try}, @litchar{catch},
and @litchar{finally} semantics for values other than
@racket[exn:break] instances, and errors raised through misuse of the
analogy itself. All other values are caught in terms of the
@racket[#:catcher], which may use values other than a unary exception
handler.

@racket[dispute] acts as both a generalized handler for raised and
non-raised values. If you call @racket[dynamic-pitch], then you take
the role of a pitcher where one of two narratives apply.

@itemlist[#:style 'ordered
@item{
@racket[#:batter] @racket[swing]s, and hits. All values returned from
@racket[swing] are the formal arguments of @racket[dispute].
}

@item{
@racket[#:batter] @racket[swing]s, and misses. @racket[#:catcher]
accepts a raised value. The formal arguments to @racket[dispute] are
the values returned from the procedure derived from @racket[catch].
If @racket[catch] accepts one argument, then it will be called as-is
with the raised value. If @racket[catch] is a nullary procedure, then
it is called with no argument, discarding the raised value. Otherwise,
@racket[catch] functions as @racket[(thunk catch)].
}
]
}
