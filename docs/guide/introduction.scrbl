#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Introduction}

A simple way to maintain dependencies is to check them all into source
control. Many programmers do not do this to avoid overhead, but it raises the
issue of how you can reproduce dependencies as if they @italic{were} checked
in. @project-name uses Racket to solve this problem.

In the context of @|project-name|, a @deftech{package} is a program.
Dependencies, setup instructions, scripts, and the output of @italic{other}
packages are all inputs to a package. The outputs of a package are
content-addressable files built from the inputs within a transaction. If you
have used Guix or Nix, this will seem familiar.

This approach has several benefits. It makes a package installation atomic,
meaning that an installation failure does not impact your Racket installation
or the wider system. Since inputs are essentially arguments, you can substitute
different inputs to fix dependency issues without waiting on a maintainer or
forking a repository.  These fixes include leaving out extraneous artifacts
like docs, tests, or GUI features, applying a security patch, or providing your
own implementation to a interface.

Users bind symbolic links to outputs to use in their own projects.  By using
symbolic links as references, @project-name can detect if files are eligible
for garbage collection.

Overall, @project-name is a viable piece of a continuous integration system
because of its isolated and reproducible builds.

The main tradeoff is a change in workflow for Racket programmers.  There is no
@litchar{install} command, and no @litchar{uninstall} command.  There is only a
transaction command called @litchar{do} that expects explicit
instructions. Another change is that @project-name defines no collections in a
Racket installation.  @secref{new-pkg} covers how to write @deftech{package
definitions} using the @racketmodname[xiden] language to define a
build. @secref{workspace} covers the directory used to store @|project-name|'s
files.

@section{Why use @|project-name| when @tt{raco pkg} exists?}

@tt{raco pkg} mutates the running Racket installation, such that a package
installation can behave differently depending on what you've installed
before. This is because packages and collections are two independently moving
parts, and they may conflict in ways that are difficult to reconcile. I made
@project-name specifically so that I could release new code without worrying
about how people have set up their Racket installations.

This means that @project-name is not meant to be viewed as a competitor to
@tt{raco pkg}, but a way to complement @tt{raco pkg}'s functionality. It is not
possible to change @tt{raco pkg} to do what @project-name does without breaking
backwards compatibility, and adding another mode to @tt{raco pkg} would
complicate its interface.

That being said, @project-name still depends on packages made available by
@tt{raco pkg}. In fact, @project-name can be installed using @tt{raco pkg} as
well. Think of @project-name as an extension to your Racket installation that
allows you to install other dependencies without the limitations of @tt{raco
pkg}, not as a reason to stop using @tt{raco pkg} entirely.


@section{Why use @project-name when Guix/Nix exists?}

First, Guix and Nix do not support Windows. @project-name does.

Second, @project-name offers a path to leveraging language-oriented programming
to control certain kinds of software distributions. Guix and Nix each define
DSLs, but you have less control of how more expressive programs translate to
those DSLs. This is not a problem for a Racket program.

Finally, @project-name uses a versioning scheme based on @tech{editions} and
@tech{revisions}, which supports more social arrangements than bureaucractic
rules surrounding dots and numbers.
