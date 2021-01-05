#lang scribble/manual

@require[@for-label[@except-in[xiden/pkgdef #%module-begin] syntax/module-reader]
         "../../shared.rkt"]

@title{Reader Extension}

@(defmodulelang* (xiden))

@racketmodname[xiden], as a reader extension, defines a
@racketmodname[xiden/pkgdef] module. The grammar matches that of a
@racketmodname[xiden/pkgdef] module body, because the reader is
defined as @racket[(module reader syntax/module-reader xiden/pkgdef)]
