#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Concepts}

In the context of @|binary|, a @deftech{package} is a program.
Dependencies, setup instructions, scripts, and the output of
@italic{other} packages are all @tech{inputs} to a package. The
outputs of a package are unique files and directories named after the
cryptographic hash of all inputs.

This approach has several benefits. It makes a package installation
appear atomic, because installation success or failure does not impact
your Racket installation or the wider system. The @secref{workspace}
section discusses the files that @binary @italic{does} impact.

Another benefit to functional package management is the ability to
override dependencies like you would with @racket[parameterize].  This
gives users a way to leave out artifacts like docs or tests, or to
reconfigure a downstream dependency.

Users do not write packages directly. They write @deftech{package
definitions}, which are programs that expand to packages. The behavior
of the package depends on your configuration. Doing it this way means
that you only need to share a package definition to distribute your
software. This adds an option to share packages via napkin or email,
which is often not a good idea, but really useful when it is.

That leads us to security. @project-name ships with a zero-trust
configuration. This means it will halt for what will at first seem
like bureaucratic reasons. In truth, it halts at any point it cannot
ascertain your trust in a dependency (which is, again, presumed
zero). @secref{config} covers how to configure @binary to strike your
own balance between security and convenience, but it would be best to
learn how to work with the zero-trust configuration when working with
resources from the Internet.


@section{Comparison to @tt{raco pkg} and @tt{raco setup}}

@tt{raco pkg} and @tt{raco setup} modify an existing Racket
installation to install a package and the collections that package
defines. This means that a future package installation can fail
(likely due to a conflict) depending on the state of your Racket
installation. Conflicting packages can still be installed, but doing
so requires knowledge of how Racket installations are configured and
launched.

@project-name does not modify your Racket installation, which means
it does not define collections when installing a package. The workflow
is file-oriented. You can, however, have @project-name "render" new
Racket installations in terms of installed packages.


@section{Comparison to Nix and Guix}

@project-name uses functional programming concepts to build packages,
much like Nix and Guix. Nix and Guix are system-level package managers
that can replicate an entire other operating system in terms of
packages. They can select build systems and orchastrate
cross-compilations. Both also model package installations as
unprivileged operations.

@project-name only concerns itself with dependency management for
Racket-driven projects. It assumes that the running Racket
installation is the build system, and uses the exact privileges
afforded to it by the operatiing system. If it were to cross-compile a
project, then it would only do so as a consequence of user action.
