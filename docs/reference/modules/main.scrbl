#lang scribble/manual

@require[@for-label[@except-in[xiden/pkgdef #%module-begin]]
         "../../shared.rkt"]

@title{Reader Extension}

@(defmodulelang* (xiden))

@racketmodname[xiden], as a reader extension, defines a
@racketmodname[xiden/pkgdef] module.

Any well-formed @litchar|{#lang info}| document is a well-formed
@litchar|{#lang xiden}| document, but @tt{raco setup} and @tt{raco
pkg} will not read @litchar|{#lang xiden}| documents.
