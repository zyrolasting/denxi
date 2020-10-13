#lang scribble/manual

@title{Packages}

This section covers the languages used to express @tech{package
definitions}, and the @tech{packages} created from them.

A @deftech{package} is an active instance of a @tech{package
definition}.  The difference between a @tech{package} and a
@tech{package definition} is therefore like the difference between a
process and a program, or a Docker container and a Docker image.

A @deftech{package definition} is a Racket module that combines
discovery information with a program. That program is used to create
and execute @tech{packages} to produce output files. A user declares
the inputs, outputs, and processing steps for that program with the
bindings described in this document.

@include-section{modules/main.scrbl}
@include-section{modules/pkgdef.scrbl}
@include-section{modules/package.scrbl}
