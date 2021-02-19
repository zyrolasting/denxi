#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Introduction}

Xiden is a dependency manager. Ask for something you need for your
project, and you'll get it.

Whenever you request a dependency, you'll get a symbolic link pointing
to a directory containing what you want. That's the easy part.

The hard part is actually creating those directories, because Xiden
aims to be:

@itemlist[
@item{
@bold{Deterministic.} Given the same request, Xiden will build the same directory.
}

@item{
@bold{Robust.} Power outages, failed builds, or trigger-happy use of @tt{kill -9} will not break things. Xiden always starts from a known working state, unless you go out of your way to corrupt data.
}

@item{
@bold{Configurable.} Every questionable decision Xiden makes can be changed.
}

@item{
@bold{Respectful.} Xiden does nothing that cannot be traced to explicit, affirmative consent.
}

@item{
@bold{Extensible.} When Xiden does not know what to do, it may fall back to the code you give it.
}

]

All of these traits are hard to get right, and that's where the
complexity comes in.

While you read this guide, just remember that Xiden's primary job is
to issue links to the directories it made.

This overall simplicity also makes Xiden a viable part of continuous
integration systems, or even operating systems.


@section{How Xiden Fulfils Dependencies}

Like other dependency managers, Xiden has its own idea of a
package. That is, a unit of software you want to "bring in" for the
sake of a project.

Normally you'd see packages as files with a special format, such as a
TAR file with a specific structure. In the context of Xiden, a
@deftech{package} is actually an active instance of a program called a
@deftech{package definition}. @secref{new-pkg} covers how to write
package definitions using the @racketmodname[xiden] language.

This will seem odd if you are used to thinking of packages as inert
artifacts. After all, we don't normally “execute” packages. In Xiden,
package @bold{definitions} are the inert artifacts. The difference
between a package definition and a package is like the difference
between a program or a process, or the difference between a Docker
image and a Docker container. In this sense, a package contains means,
not ends.

This program-centric view might seem like it complicates things, but
it actually allows us to simplify everything else in terms of
@bold{inputs} and @bold{outputs}. Dependencies, setup instructions,
scripts, and the output of @italic{other} packages are all possible
inputs to a package. The outputs of a package are content-addressable
files built within a transaction. If you have used Guix or Nix, this
will seem familiar.

The framework of input-processing-output makes it easier to define
installation as an atomic, pure function. If inputs are just
arguments, you can substitute different inputs to fix dependency
issues without waiting on a maintainer or forking a repository.  These
fixes include leaving out extraneous artifacts like docs, tests, or
GUI features, applying a security patch, or providing your own
implementation to a interface.  Fixing a failure amounts to running
the function again.

When Xiden builds package outputs, they will appear in directories.
These are the directories that Xiden will use when creating symbolic
links. When a user asks for a dependency and is issued a link to the
right directory, the user's request is fulfiled.


@section{Why Symbolic Links?}

Symbolic links decouple Xiden's internal addressing scheme from your
own. If you want the documentation for package X, then you can link to
it as @litchar{x-docs} in your project. Meanwhile, Xiden can see all
of the links it issued and see that which ones point to the same
place.  This is good for garbage collection. No incoming links? Outta
here.

This method also helps prevent name collisions. If two people make a
package with the same name, you can still install them both and issue
links with names that clarify their purpose.


@section{What's the Catch?}

Xiden can be inconvenient to use out of the box. This is easily
fixable, but you should know why you have to be the one to fix it.

Xiden defaults to a zero-trust model for security. It is installed on
your computer with the firmly-held belief that everything and everyone
is out to get you. This means that it won't trust public keys and
executables unless you say you trust them.  If you prioritize
convenience over security, then you have to configure Xiden to be less
cautious.

This is the same thinking behind many firewalls. You start with a
"Deny All" rule, then add rules that reflect intended use. Starting
with "Permit All" for convenience might get you going faster, but it's
@hyperlink["http://www.ranum.com/security/computer_security/editorials/dumb/"]{a
really bad idea}. When you apply permissiveness to package managers,
you get things like NPM.

On a less serious note, you'll have to deal with a small change in
workflow. There is no @litchar{install} or @litchar{uninstall}
command.  There's a transaction command to add software, and a garbage
collection command to remove what isn't being used. In that light,
updates and rollbacks are just making symbolic links point to
different directories.

If you are writing Racket programs, note that Xiden has no side-effect
on your Racket installation. This means it defines no collections.  If
you want to reference installed software as if it were a collection,
then you'll need to define the collection yourself.


@section{Why use Xiden when @tt{raco pkg} exists?}

@tt{raco pkg} mutates the running Racket installation, such that a
package installation can behave differently depending on what you've
installed before. Once installed, Xiden is immune to the consequences
of this behavior.

It is not possible to change @tt{raco pkg} to do what Xiden does
without breaking backwards compatibility, and adding another mode to
@tt{raco pkg} would overcomplicate its interface. If you run into a
limitation with @tt{raco pkg}, then you are probably better served by
using Xiden than trying to fix @tt{raco pkg}.

To be clear, Xiden is not meant to compete with or replace @tt{raco
pkg}. Xiden still depends on packages made available by @tt{raco
pkg}. In fact, Xiden can be installed using @tt{raco pkg} as
well. Think of Xiden as an extension to your Racket installation that
allows you to install other dependencies without the limitations of
@tt{raco pkg}, not as a reason to stop using @tt{raco pkg} entirely.


@section{Why use Xiden when Guix and Nix exist?}

First, Guix and Nix do not support Windows. Xiden does.

Second, Xiden offers a path to leveraging language-oriented
programming to control certain kinds of software distributions. Guix
and Nix each define DSLs, but you control what DSLs are in the first
place when using Racket.

Finally, Xiden uses a versioning scheme based on @tech{editions} and
@tech{revisions}, which supports more social arrangements than
bureaucractic rules surrounding dots and numbers.
