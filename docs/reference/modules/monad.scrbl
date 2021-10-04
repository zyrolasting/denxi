#lang scribble/manual

@require[@for-label[racket
                    denxi/archive
                    denxi/input
                    denxi/subprogram
                    denxi/monad
                    @only-in[denxi/pkgdef output]]
          "../../shared.rkt"]


@title{Monads}

@defmodule[denxi/monad]

@racketmodname[denxi/monad] implements a monomorphic bind operation
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

In the simplest case, @racket[(void? (mdo))]. This captures the
sequence that does nothing.

For single-terms, @racket[(eq? (mdo V) V)]. Once more than one term is
available, each term must be of the same monadic type.  In other
words, @racket[mdo] is monomorphic.

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



@section[#:tag "monad-examples"]{Examples and Background}

Some subprograms in Denxi use an alternative notation within a
@racket[output] or @racket[mdo] form.

@racketblock[
(output "default"
        archive-input := (input-ref "default.tgz")
        path := (resolve-input archive-input)
        (extract path)
        (release-input archive-input))
]

@racket[:=] is like @racket[let], but it isn't @italic{exactly} the
same because this abbreviated program does not work.

@racketblock[
(output "default"
        (release-input (extract (resolve-input (input-ref "default.tgz")))))
]

This program breaks because @racket[:=] does more than bind a value to
an identifier.  It also finds the value it needs to bind in the first
place from a special context called a @deftech{monad}.  There are many
tutorials that explain monads poorly, and this is likely one of
them. I encourage you to search online for programming exercises, but
I'll give you an abridged introduction to keep you moving in Denxi's
documentation.

Here are two functions.

@racketblock[
(define (add5 v)
  (+ v 5))

(define (sub2 v)
  (- v 2))
]

You can compose them.

@racketblock[
(sub2 (add5 4))
]

Now let's change the functions so that they return another value.

@racketblock[
(define (add5 v) (values (+ v 5) (format "Adding 5 to ~s" v)))
(define (sub2 v) (values (- v 2) (format "Subtracting 2 from ~s" v)))
]

Functional programmers might do this because everything returned from
each function is expressed purely in terms of arguments, and there are
no side-effects.

Problem is, @racket[(sub2 (add5 4))] no longer works, and we have not
defined a way to separate and handle the first and second values where
they each make sense. @tech{Monads} takes care of all that, so
function composition works again.

The @racket[mdo] form knows how to perform composition in this way,
and the @racket[:=] operator knows how to extract the actual value you
want to use from the extra data. therefore, @racket[(extract
(resolve-input (input-ref "default.tgz")))] doesn't work because you
passed the value you want plus extra @italic{stuff} to
@racket[extract].

If you look at the documentation for a function in Denxi and see that
it returns an unfamiliar value like @racket[(subprogram/c string?)]
instead of just @racket[string?], it is probably meant for use as a
monadic type, where @racket[:=] pulls out the string you want.
