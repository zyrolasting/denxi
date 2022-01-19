#lang scribble/manual

@require[@for-label[denxi/base]]

@title{Base}

@defmodule[denxi/base] reprovides @racketmodname[racket/base] and
@racketmodname[racket/contract]. It is used internally to extend
various modules in the @tt{racket} collection.

@defform[(reprovide module-path ...)]{
@racket[require] and @racket[provide] all bindings in each
@racket[module-path].
}
