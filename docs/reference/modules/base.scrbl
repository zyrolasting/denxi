#lang scribble/manual

@require[@for-label[denxi/base]]

@title{Base}

@defmodule[denxi/base] reprovides @racketmodname[racket/base] and
@racketmodname[racket/contract]. It is used internally as an
@racket[s-exp] language to help abbreviate Denxi modules.

@defform[(reprovide module-path ...)]{
@racket[require] and @racket[provide] all bindings in each
@racket[module-path].
}
