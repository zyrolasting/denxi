#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Concepts}

Dependency management is a way to produce exact bytes with limited
information. In the context of @|project-name|, a @deftech{package} is
a program.  Dependencies, setup instructions, scripts, and the output
of @italic{other} packages are all @tech{inputs} to a package. The
outputs of a package are content-addressable files built from the
inputs within a transaction.

This approach has several benefits. It makes a package installation
atomic, meaning that an installation failure does not impact your
Racket installation or the wider system. The @secref{workspace}
section discusses the files that @binary @italic{does} impact.

You can override package inputs like you would with
@racket[parameterize].  This gives you a way to leave out artifacts
like docs or tests, or to replace a dependency.

@secref{new-pkg} covers how to write @deftech{package definitions}
using the @racketmodname[xiden] language.

@project-name ships with a zero-trust configuration, meaning it
will halt at any point it cannot ascertain your trust in a
dependency. @secref{config} covers how to configure @binary to strike
your own balance between security and convenience, but it would be
best to learn how to work with the zero-trust configuration when
working with resources from the Internet.
