#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Introduction}

A simple way to maintain dependencies is to check them all into source
control. Many programmers do not do this for obvious reasons, but it
raises the issue of how you can reproduce dependencies as if they
@italic{were} checked in. @project-name uses Racket to solve this
problem.

In the context of @|project-name|, a @deftech{package} is a program.
Dependencies, setup instructions, scripts, and the output of
@italic{other} packages are all @tech{inputs} to a package. The
outputs of a package are content-addressable files built from the
inputs within a transaction. Users bind symbolic links to outputs to
use in their own projects.

This approach has several benefits. It makes a package installation
atomic, meaning that an installation failure does not impact your
Racket installation or the wider system. Since inputs are essentially
arguments, you replace them to fix dependency issues without waiting
on a maintainer or forking a repository.  This also gives you a way to
leave out extraneous artifacts like docs, tests, or GUI
features. Using symbolic links as references also creates a way to
detect if files are eligible for garbage collection. @project-name is
a viable piece of a continuous integration system because of its
isolated and reproducible builds.

The main tradeoff is a change in workflow for Racket programmers.
There is no @tt{install} command, and no @tt{uninstall} command.
@project-name also defines no collections in a Racket installation.
@secref{new-pkg} covers how to write @deftech{package definitions}
using the @racketmodname[xiden] language to define a
build. @secref{workspace} covers the directory used to store
@|project-name|'s files.

@project-name is @bold{not yet production ready}. Dependency
management is a hard problem, and it takes time to do well. It
currently trusts some inputs when it shouldn't, simply because
signature/certificate checking is not complete. Until it is , you will
need to exercise caution when using code from the
Internet. @project-name is on the way to a zero-trust configuration,
meaning it will take every precaution it can. @secref{config} covers
how to configure @binary to strike your own balance between security
and convenience, but it would be best to learn how to work with the
zero-trust configuration as it develops.
