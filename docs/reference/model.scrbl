#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Model}

Xiden uses @tech{package definitions} to build directories and issue
symbolic links to those directories.  A garbage collector deletes any
directory it created once it has no known symbolic links. When given
the oppurtunity to perform file or network I/O, Xiden will proceed
only with the user's consent.

Since dependency management is subjective, Xiden requires users to
define @tech{launchers}. This allows users to set configurations and
conventions in advance of Xiden's role in their projects.

This section covers the modules that cooperate to these ends.

@table-of-contents[]
@include-section{modules/pkgdef.scrbl}
@include-section{modules/package.scrbl}
@include-section{modules/main.scrbl}
@include-section{modules/version.scrbl}
@include-section{modules/query.scrbl}
@include-section{modules/localstate.scrbl}
@include-section{modules/source.scrbl}
@include-section{modules/input.scrbl}
@include-section{modules/artifact.scrbl}
@include-section{modules/launcher.scrbl}
@include-section{modules/setting.scrbl}
@include-section{verification.scrbl}
