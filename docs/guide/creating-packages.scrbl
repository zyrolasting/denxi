#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title[#:tag "new-pkg"]{Creating Packages}

Use the @tt{new} command to create a package.  You should see a new
directory with the name you provided.

@verbatim|{
$ xiden new my-pkg
$ ls my-pkg
}|

@section{The Package Definition File}

Inside that directory you should see @|definition|. This is your
@deftech{package definition}, and it looks something like this:

@racketmod[
info

(define package "foo")
(define description "Describe this package in a sentence.")
(define tags '())
(define home-page "https://example.com")
(define edition "draft")
(define revision-number 0)
(define revision-names '())
(define provider "localhost.localdomain")
(define setup-module "setup.rkt")
(define dependencies '())
(define launchers '())
(define racket-versions '(("6.0" . "7.7.0.5")))
]

If the @racket[provider] name is different, then that's okay.  It is set to the
hostname of your machine.

@subsection{How a Query Matches a Package Definition}

For this example, the @tech{query} @tt{localhost.localdomain:my-pkg:draft:0}
refers to this package. You can see that a query consists of the
@racket[provider], @racket[package], @racket[edition], and
@racket[revision-number] fields.

If you set @racket[revision-names] to @racket['("initial" "oldest"
"beginning")], then users can replace the @racket[0] in their query with any of
those strings and still get this package. Choose revision names wisely: Once
you host the package, you will not be allowed to use the same revision names
again for the edition.

@subsection{Declare Dependencies}

You can set @racket[dependencies] using the same strings discussed in
@secref{asking}.

@racketblock[(define dependencies '("john.doe:calculator" "alice:wonderland:small"))]

@subsection{Declare Supported Racket Versions}

@racket[racket-versions] is a list of pairs, where each pair is an inclusive
interval of Racket versions you support for this package. Gaps in versions
are not expected, but you can express them for flexibility.

@subsection{Help Others Discover This Package}

@racket[tags] is a list of strings you can set for discovery purposes when
users search for packages.

@racketblock[(define tags '("video game" "3d" "action"))]

@racket[description] is a short summary of your package. It will also be
checked in user searches.

@racketblock[(define description "A shoot-em up game built with 'universe'")]

@racket[home-page] is an URL to a related web page for your package.

@subsection{Make a First Impression}

@racket[setup-module] is a path relative to the @tech{package definition}.
It points to the other file the @tt{new} command made for us.  That
file is a @deftech{setup module}. It automates optional tasks on
behalf of the user, such as installing non-Racket dependencies.

The setup module also acts as a conventional entry point for a user
that does not (yet) trust a package. To understand what I mean, let's
reverse the roles where you are the user of a package and not the
author. If you are suspicious of an installed package, you can
start a sandboxed REPL in the context of a setup module using the
@tt{sandbox} command.

@verbatim|{
$ xiden sandbox mavrick:pyracket:totally-works
>
}|

By default, the sandbox prohibits all file and network I/O, and limits
both the time and memory the setup module can consume. Since the
sandbox uses your kernel, it is not a substitute for OS-level
security.  See @racketmodname[racket/sandbox] for more information.

The @tt{new} command also creates a conventional @tt{show-help} procedure in the
setup module. It serves to educate and negotiate permissions with a human user.

@verbatim|{
> (show-help)
"Run (install-python3) with write permissions to [...],
and permission to download from [***]"
^D
$ xiden sandbox --sandbox-path-permissions "[...]" \
                --sandbox-network-permissions "[***]" \
                -e "(install-python3)" \
                mavrick:pyracket:totally-works
}|


@section{Developing Packages Locally}

You can install your local package in a @tech{workspace} like so:

@verbatim|{
$ xiden install -y my-pkg
}|

By default, this copies your package files to the workspace. If
something does not work right, you have to first uninstall the package
using the associated query before installing the package again.

@verbatim|{
$ xiden uninstall -y localhost.localdomain:my-pkg
$ xiden install -y my-pkg
}|

To save some time, you can instead install the package using a
symbolic link. In this case, your changes will immediately reflect on
the workspace.

@verbatim|{
$ xiden install -y --link my-pkg
}|

When using a link, @binary will use the original source when
setting up your package according to @secref{setup}.

@section[#:tag "setup"]{Package Setup}

@margin-note{@litchar{xiden setup} is equivalent to @litchar{raco setup} in purpose.}

Use the @litchar{setup} command to (re)integrate a package with your
system. Setup runs automatically when you install a package, but you
may need to run setup again manually whenever you change the source
code of a package available in the workspace.

@verbatim|{
$ xiden setup localhost.localdomain:my-pkg
}|

Specifically, the @litchar{setup} command:

@itemlist[
@item{Creates launchers for a package}
@item{Compiles @tt{.rkt}, @tt{.ss}, and @tt{.scrbl} modules to bytecode}
@item{Creates symbolic links to depedencies in @depdir}
@item{Creates symbolic links to revisions within the @tech{workspace}.}
]
