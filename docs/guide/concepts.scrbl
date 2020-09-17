#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Introduction}

A simple way to maintain dependencies is to check them all into source
control. Many programmers do not do this to avoid overhead, but it
raises the issue of how you can reproduce dependencies as if they
@italic{were} checked in. @project-name uses Racket to solve this
problem.

In the context of @|project-name|, a @deftech{package} is a program.
Dependencies, setup instructions, scripts, and the output of
@italic{other} packages are all @tech{inputs} to a package. The
outputs of a package are content-addressable files built from the
inputs within a transaction.

This approach has several benefits. It makes a package installation
atomic, meaning that an installation failure does not impact your
Racket installation or the wider system. Since inputs are essentially
arguments, you can subtitute different inputs to fix dependency issues
without waiting on a maintainer or forking a repository.  These fixes
include leaving out extraneous artifacts like docs, tests, or GUI
features, applying a security patch, or providing your own
implementation to a interface.

Users bind symbolic links to outputs to use in their own projects.  By
using symbolic links as references, @project-name can detect if files
are eligible for garbage collection.

Overall, @project-name is a viable piece of a continuous integration
system because of its isolated and reproducible builds.

The main tradeoff is a change in workflow for Racket programmers.
There is no @litchar{install} command, and no @litchar{uninstall}
command.  There is only a transaction command called @litchar{do} that
expects explicit instructions. Another change is that @project-name
defines no collections in a Racket installation.  @secref{new-pkg}
covers how to write @deftech{package definitions} using the
@racketmodname[xiden] language to define a build. @secref{workspace}
covers the directory used to store @|project-name|'s files.
