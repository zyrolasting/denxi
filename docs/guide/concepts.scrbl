#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Concepts}

In the context of @|binary|, a @deftech{package} is a program that
behaves like a pure function.  Dependencies, setup instructions,
scripts, and the output of @italic{other} packages are all
@tech{inputs} to a package. The outputs of a package are unique files
and directories named after the cryptographic hash of all inputs.

This approach has several benefits. It makes a package installation
appear atomic, because installation success or failure does not impact
your Racket installation or the wider system. The @secref{workspace}
section discusses the files that @binary @italic{does} impact.

Another benefit to functional package management is the ability to
override dependencies like you would with @racket[parameterize].  This
gives users a way to leave out artifacts like docs or tests, or to
reconfigure a downstream dependency.

Users do not write packages. They write @deftech{package definitions},
and @binary will create packages from them. The behavior of the
package depends on your configuration. Doing it this way means that
you only need to share a package definition to distribute your
software. This lessens the need for catalogs, and opens up Pastebin,
Gists, and other clear text mediums as ways to share work.

Security is part of @|binary|'s design, and it ships with a zero-trust
configuration. This means it will halt and raise errors at times most
people may find inconvenient. @secref{config} covers how to configure
@binary to strike your own balance between security and convenience,
but it would be best to learn how to work with the zero-trust
configuration when working with resources from the Internet.
