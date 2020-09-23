#lang scribble/manual

@require[@for-label[racket/base]
         "../shared.rkt"]

@title{Package Definition Language}

This section covers the languages used to express @tech{package
definitions}.

A @deftech{package definition} is a Racket module that combines
discovery information with a rough description of a program.  A
package user or author would declare the inputs, outputs, and
processing steps for that program with the bindings described in this
document.

@section{Module Language}

@defmodule[xiden/derivation-forms]

@racket[xiden/derivation-forms] is a module language. It is a superset
of @racketmodname[setup/infotab].

@section{Reader Extension}

@(defmodulelang* (xiden))

@racketmodname[xiden], as a reader extension, defines a
@racketmodname[xiden/derivation-forms] module. Any well-formed
@litchar|{#lang info}| document is a well-formed @litchar|{#lang
xiden}| document.

@tt{raco setup} and @tt{raco pkg} will not read @litchar|{#lang
xiden}| documents.
