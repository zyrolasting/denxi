#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Concepts}

In the context of @|binary|, a @deftech{package} is a program.  This
view considers dependencies and setup instructions as
@tech{inputs}. The output is a unique directory named after the
cryptographic hash of all inputs.

This approach has several benefits. It makes a package installation
@italic{appear} atomic, because a package can never be considered
installed @italic{unless} it succeeds. This is preferable to a package
management system that leaves a larger system in a broken
state. Repairing a package distribution, or simply trying a new
version, is a matter of executing a package again, possibly with
different inputs.

Another benefit is the ability to @racket[parameterize] inputs,
such that a package can use a slightly reconfigured variant of
a downstream dependency.

@binary generates packages from @deftech{package definitions}, and
interprets their instructions in a sandbox. This is because packages
are typically meant for sharing, and packages created from
Internet-sourced data can be dangerous. For safety, @binary ships with
a zero-trust configuration. This means it will halt and raise errors
at times most people will find inconvenient. @secref{config} covers
how to configure @binary to strike your own balance between security
and convenience.

@binary also concerns itself with managing the output of all packages.
Unlike @tt{raco pkg}, @binary packages do not define collections in a
Racket installation.  The @secref{workspace} section discusses how
@binary manages the totality of its impact on a system.
