#lang scribble/manual

@require[@for-label[racket
                    xiden/archive
                    xiden/input
                    xiden/logged
                    xiden/monad
                    @only-in[xiden/pkgdef output]]]

@title[#:tag "monads"]{An Informal Overview of Xiden's Monadic “Do” Notation}

Some subprograms in Xiden use an alternative notation within a
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
I'll give you an abridged introduction to keep you moving in Xiden's
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

If you look at the documentation for a function in Xiden and see that
it returns an unfamiliar value like @racket[(logged/c string?)]
instead of just @racket[string?], it is probably meant for use as a
monadic type, where @racket[:=] pulls out the string you want.
