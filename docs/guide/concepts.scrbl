#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Concepts}

In the context of @|binary|, a @deftech{package} is a program.
Dependencies, setup instructions, scripts, and packages are all
@tech{inputs} to this program. The outputs of a package are unique
files and directories named after the cryptographic hash of all
inputs. Users of Guix and Nix should find this familiar.

This approach has several benefits. It makes a package installation
appear atomic, because installation success or failure does not impact
your Racket installation or the wider system. Repairing a package
distribution, or simply trying a new version, is a matter of executing
a package again. The @secref{workspace} section discusses the files
that @binary @italic{does} impact.

Another benefit to functional package management is the ability to
override dependencies like you would with @racket[parameterize].  This
gives users a way to leave out artifacts like docs or tests, or to
reconfigure a downstream dependency.

@margin-note{Guix/Nix users: The main difference between @binary and
your preferred tool is that Racket is the presumed build system.}
Users do not write packages. They write @deftech{package definitions},
and @binary will create packages from them. The behavior of the
package depends on your configuration. Doing it this way means that
you only need to share a package definition to distribute your
software. This lessens the need for catalogs. It also makes Pastebin,
Gists, and other clear text mediums more useful for sharing work.

Security is part of @|binary|'s design, and it ships with a zero-trust
configuration. This means it will halt and raise errors at times most
people may find inconvenient. @secref{config} covers how to configure
@binary to strike your own balance between security and convenience,
but it would be best to learn how to work with the zero-trust
configuration when working with resources from the Internet.
