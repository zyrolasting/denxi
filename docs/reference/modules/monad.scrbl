#lang scribble/manual

@require[@for-label[racket/base racket/contract xiden/subprogram xiden/monad]
          "../../shared.rkt"]

@title{Monads}

@defmodule[xiden/monad]

@racketmodname[xiden/monad] implements a monomorphic bind operation
and a notation for composition.

@defidform[gen:monad]{
A generic interface for monad types.

Only includes @racket[bind] to date.
}

@defthing[monad? predicate/c]{
Returns @racket[#t] if the argument implements @racket[gen:monad].
}

@defproc[(bind [ma monad?] [lift (-> any/c monad?)]) monad?]{
Returns a monadic value that composes @racket[ma]
with the monadic value returned from @racket[lift].

Use to compose two functions that each deal with a monadic type.
}

@defform[(mdo step ...)
         #:grammar [(step expr
                          (code:line id := expr))]]{
A “monadic do” form for composing a sequence of steps using bind
operations.

In the simplest case, @racket[(eq? (mdo V) V)]. Once more than one
term is available, each term must be of the same monadic type.  In
other words, @racket[mdo] is monomorphic.

@racket[:=] is the monadic bind operator. Here's an example in terms
of @tech{subprograms}:

@racketblock[(mdo number := (subprogram-unit 1)
                  number)]
}

@defidform[:=]{
Binds an expression on the right hand side to an identifier on the
left hand side in the context of @racket[mdo].

Raises an error when used outside of @racket[mdo]. Since @tech{package
outputs} use implicit @racket[mdo] forms, @racket[:=] may also appear
within them.
}
