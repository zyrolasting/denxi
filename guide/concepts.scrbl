#lang scribble/manual

@require["shared.rkt" @for-label[racket/base]]

@title{Concepts}

@binary concerns itself with the safe, deterministic reproduction of
exact @tech{packages} and dependencies without any side-effect on the
running Racket installation. In that light, it applies aspects of
functional programming to package management.

A @deftech{package} is a directory containing a @tech{package
definition}. Packages can be installed on a local system, or bundled
for distribution over a network. Unlike packages defined for @tt{raco
pkg}, @binary packages do not define collections in a Racket
installation.

All of @|binary|'s effects on a system are restricted to a single
directory. This directory is called a @deftech{workspace}. See
@secref{workspace} for more information.

A user must provide a @deftech{query} to find a package. An
@deftech{exact query} is expected to match @italic{the same}
implementation of a package @italic{every time}, whereas an
@deftech{inexact query} can match different (yet presumably
equivalent) implementations.  An exact query behaves like an ISBN does
for books: If you send any two servers the same @tech{exact query},
then you will get the same information. It doesn't matter if the
servers use JSON, XML, or HTML to express the packages or their
metadata. It doesn't matter if the server consults GitHub, a
filesystem, S3, or a sacrificial altar. A server's job is only to
resolve @tech{queries} to packages or related metadata. A server does
not need to host everything because of the ISBN-like relationship
between @tech{exact queries} and packages. @binary, as a client,
merely needs to consult a second server in the event the first cannot
fulfill a request for information.

There exists room for human error that make determinism hard to
guarentee.  The @secref{verification} section covers tools and
techniques for reproducing a bit-for-bit copy of a @tech{workspace}.
