#lang scribble/manual

@require[@for-label[@except-in[denxi/pkgdef #%module-begin] syntax/module-reader]
         "../../shared.rkt"]

@title{Reader Extension}

@(defmodulelang* (denxi))

@racketmodname[denxi], as a reader extension, defines a
@racketmodname[denxi/pkgdef] module. The grammar matches that of a
@racketmodname[denxi/pkgdef] module body, because the reader is
defined as @racket[(module reader syntax/module-reader denxi/pkgdef)]
