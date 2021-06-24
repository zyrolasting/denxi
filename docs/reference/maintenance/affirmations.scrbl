#lang scribble/manual

@require[@for-label[racket/base] "../../shared.rkt"]

@title{Affirmations}

Each security-critical check is implemented using a composition of
@deftech{affirmations}.  An @tech{affirmation} is a Racket procedure
that meets several requirements.

@itemlist[
@item{The procedure is pure}
@item{The procedure uses continuation-passing style}
@item{The procedure has a cyclomatic complexity of exactly @racket[2].}
@item{The procedure has no default arguments}
@item{If the procedure returns a value without applying a continuation,
      then that value contains the procedure's name.}
]

For example, let's look at a procedure that returns @racket[#t] if a
real number is an element of an exclusive interval.

@racketblock[
(define (num-between n lo hi)
  (and (> n lo) (< n hi)))
]

If we define the computation with @tech{affirmations}, the code
becomes far more verbose.

@racketblock[
(define-message $between (ok? stage))

(define (affirm-< n hi continue)
  (if (< n hi)
      (continue)
      ($between #f (object-name affirm-<))))

(define (affirm-> n lo continue)
  (if (> n lo)
      (continue)
      ($between #f (object-name affirm->))))

(define (num-between n lo hi)
  (affirm-> n lo
            (lambda ()
              (affirm-< n hi
                        (lambda ()
                          ($between #t (object-name num-between)))))))
]

This form returns an answer, any desired context, and the name of the
procedure responsible for the answer in a single value. The original
@racket[num-between] would tell us if a number is not an element of
@litchar{(lo, hi)}, but the modified version tells us @italic{why} it
isn't. If we see @racket[(object-name affirm-<)], then we know that
the number was too large for the interval without using @racket[<] or
keeping a reference to @racketid[hi].

In the event an @tech{affirmation} allows a dangerous operation, a
program log will point to it.  The goal is to offer transparent logs,
reduced debugging time, and more accessible testing at the expense of
more verbose code.
