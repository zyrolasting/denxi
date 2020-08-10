#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Adding and Removing Packages}

You can add and remove packages using @tech{queries}.

@verbatim|{
$ zcpkg install -y john.doe:calculator
$ zcpkg uninstall -y john.doe:calculator
}|

Unless forced, @binary will not install any suspicious packages.  If a
situation comes up, then @binary will explain your options.

Don't worry about conflicting versions. @binary installs packages side-by-side,
meaning that two versions of the same package each get a unique directory.

When @binary installs a package, it adds a @|depdir| directory to the package's
installation directory (See @secref{new-pkg}). Package authors use this
directory to access package modules using the package provider's name and the
package's name.

@racketblock[
(require (file "zcpkg-deps/john.doe/calculator/module.rkt"))
]

This illustrates the fundamental difference between @binary and
@tt{raco pkg}: @binary does not define
@tech/reference{collections}. However, packages can define launchers
that map top-level collection names to dependency directories.

You can access the packages you installed for a specific use, outside of
conventions set by @|binary|. To do this, use the @litchar{link} command to
make a symbolic link to a package installed on your system.

@verbatim|{
$ zcpkg link john.doe:calculator calc
$ ls calc
}|
