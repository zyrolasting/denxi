#lang scribble/manual

@require["shared.rkt"]

@title{Concepts}

A @deftech{package} is a directory containing a @tech{package definition}.

Unlike packages defined for @tt{raco pkg}, @binary packages do not define
collections in a Racket installation. @binary concerns itself with the safe,
deterministic reproduction of source files and their dependencies in a
@deftech{workspace} directory, without any side-effect on the running Racket
installation. @secref{workspace} covers how a workspace decouples a running
system from any installed packages.

Also unlike @tt{raco pkg}, there is no concept of a package source, in
the sense that a single string could refer to a catalog-friendly name,
a URL to a GitHub repository, or a path on the file-system.  A user
must provide a @deftech{query} to find a package. An @deftech{exact
query} is expected to match @italic{the same} implementation of a
package @italic{every time}, whereas an @deftech{inexact query} can
match different (yet presumably equivalent) implementations.  An exact
query behaves like an ISBN does for books: If you send any two servers
the same @tech{exact query}, then you will get the same information.

It doesn't matter if the servers use JSON, XML, or HTML to express the packages
or their metadata. It doesn't matter if the server consults GitHub, a
filesystem, S3, or a sacrificial altar. A server's job is only to resolve
@tech{queries} to packages or related metadata. A server does not need to host
everything because of the ISBN-like relationship between @tech{exact queries}
and packages. @binary, as a client, merely needs to consult a second server in
the event the first cannot fulfill a request for information.

There exists room for human error that make determinism hard toq
guarentee.  The @secref{verification} section covers tools and
techniques for reproducing a bit-for-bit copy of a @tech{workspace}.
