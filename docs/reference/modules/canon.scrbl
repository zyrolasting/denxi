#lang scribble/manual

@require[@for-label[racket/base racket/contract racket/undefined]
         denxi/canon
         "../../shared.rkt"]

@title{Canonicalization}

@defmodule[denxi/canon]

Using Owen Barfield's and George Boole's vocabulary, every value
produced by a human is simultaneously a @search-term{logomorphism} and
@search-term{wholistic reference}. @deftech{Canonicalization}, then,
is the act of translating wholistic references between domains of
discourse until the user believes the value can exist without further
clarification.

Use @defmodname[denxi/canon] to operate on a value representing the
semantics of another value. A non-canonical Racket value may be
@racket[eq?] to a canonical value, by virtue of being the output of a
@tech{canon}.

@defthing[canon/c #:value (recursive-contract (-> any/c any/c canon/c any/c)]{
A @deftech{canon} is a procedure that returns a @deftech{canonical}
value.

The first argument is a value representing a domain of discourse (e.g. @racket['xbox-names])

The second argument is a value representing an element of the first argument. (e.g. @racket{360})

The third argument is a @tech{canon}, possibly @racket[eq?] to the
called canon.
}


@defthing[current-canon (parameter/c canon?)]{
Controls a default argument of @racket[canonicalize].

Defaults to @racket[default-canon].
}


@defthing[default-canon canon/c]{
A @tech{canon} that simply returns its element argument.
}

@defproc[(canonicalize [domain any/c] [value any/c] [canon canon/c (current-canon)]) any/c]{
Equivalent to @racket[(canon domain value canon)].
}
