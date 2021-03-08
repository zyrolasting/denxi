#lang scribble/manual

@require[@for-label[racket
                    xiden/strict-rc]
          "../../shared.rkt"]

@title{Restricted Runtime Configuration Access}

@defmodule[xiden/strict-rc]

@racketmodname[xiden/strict-rc] provides a simplified interface for
managing @tech{runtime configurations} @italic{strictly} during
runtime. This circumvents static dependency cycles involving
@racketmodname[xiden/rc], which is helpful because
@racketmodname[xiden/rc] depends on many other modules in the
collection.

@defthing[rc-key? predicate/c]{
Returns @racket[#t] when the sole argument is a key of @racket[XIDEN_SETTINGS].
}

@defproc[(rc-ref [key rc-key?]) any/c]{
Returns the value bound to a setting with the given @racket[key].
}

@defproc[(rc-rebind [key rc-key?] [value any/c] [continue (-> any)]) any]{
Calls @racket[continue]. While control remains in the context of
@racket[continue], @racket[(rc-ref key)] is @racket[eq?] to
@racket[value].
}
