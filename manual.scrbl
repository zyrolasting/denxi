#lang scribble/manual
@require[@for-label[racket/base]
         "workspace.rkt"]

@title{A Zero-Collection Package Management System}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

@(define depdir @(litchar @CONVENTIONAL_WORKSPACE_NAME))
@(define binary @(tt "zcpkg"))
@(define specfile @(litchar "zcpkg.rktd"))

This guide covers the @binary package manager. For why we need
another package manager, see @other-doc["white-paper.scrbl"].

@section{Adding and Removing Packages}

As expected, you can create and install packages on your filesystem
for local development.

@verbatim|{
$ zcpkg new my-package
$ zcpkg install -y ./my-package
}|

Once you want to fetch packages over a network, you request the
packages using an URN. At the command line, you use abbreviated forms
of these URNs to capture an acceptable version range for a package.

@verbatim|{
$ zcpkg install -y sagegerard.com:polyglot:draft
$ zcpkg uninstall -y john.doe:albegra:teacher
}|

@binary will not install any packages that do not pass an integrity
check, or that do not have a valid digital signature. You can override
this behavior, but even then, no packages are allowed to execute on
installation.

Packages are installed side by side, such that no two versions can
conflict on disk. Updating a package is a matter of installing a
new version of a package you want and then uninstalling the version
you do not wish to keep. If a package on the system insists on keeping
the old version, @binary will alert you of this.


@section{The Workspace}

All of @tt{zcpkg}'s disk activity takes place in a @deftech{workspace
directory} called @|depdir|. @depdir starts empty, but added files are
organized according to the
@hyperlink["https://refspecs.linuxfoundation.org/FHS_3.0/fhs/index.html"]{Filesystem
Heirarchy Standard v3.0}.

When there is no room for confusion, I will call this directory the
@deftech{workspace}. @binary will first check to see if @depdir is a
subdirectory of @racket[(current-directory)].  Failing that, it will
check every parent directory in a walk towards a root directory. If
@depdir does not exist, then @binary will assume that it needs to
create a new one in @racket[(current-directory)].

When I refer to a path using a leading @litchar{:}, as in
@litchar{:/usr/local}, I am referring to a path relative to the
@depdir used by the package manager.

Doing it this way yields some properties:

@itemlist[

@item{Everything @tt{zcpkg} does is in this directory, so there isn't
any mystery about where caches or packages are.}

@item{Any one of your projects can have its own @depdir, and therefore
its own configuration and dependencies. Each @depdir is fully isolated
unless you go to your own lengths to link them together.}

@item{A workspace is a valid target for @tt{chroot}. @tt{zcpkg} can
use Racket packages to construct an operating system or jailed
application.}

@item{You can define the actual root directory of your system
as a workspace for system-wide impact.}

]


You can also serve your installed packages. Clients using zcpkg can
then use your IP as a host for packages using the same URNs.

@verbatim|{
$ zcpkg serve
}|


@subsection{Configuration}

Both the package manager and service use a configuration space derived
from English labels of individual settings.

Let a hypothetical setting be “Use Widget”, which is a boolean.

In this case, the value of this setting is the last item in this list
to define a value:

@itemlist[#:style 'ordered

@item{A hard-coded default.}

@item{A @racket[read]-able literal in @litchar{:/etc/zcpkg.rktd}.}

@item{An environment variable named @tt{ZCPKG_USE_WIDGET}.}

@item{A command-line flag named @tt{--use-widget}. If the value is a
boolean, the flag itself will suffice. Otherwise it is a string
containing a @racket[read]-able value as a literal. Shortened
command-line flags are defined independently.}

]

Documented settings use links to documented parameters using their
capitalized name. e.g. @racket[ZCPKG_INSTALLER_MEMORY_LIMIT_MB]. These
links will appear even in interface definitions that would format the
name differently.

@subsection{Spec Files}

Like @tt{info.rkt}, ZCPs have a file for package metadata.
It's called @|specfile|. Options are defined as an alternating
sequence of keywords and Racket literals.

@racketblock[
#:provider "sagegerard.com"
#:package "tweedledee"
#:edition "draft"
#:revision-number 12
#:revision-names ("2020-01-18" "open-beta")
#:dependencies ("../tweedledum")
#:setup-module "setup.rkt"
]


@subsection{Sandboxing Packages and Opt-in Setup}

A ZCP's @tech{spec file} may name an @deftech{setup module} that
performs optional tasks on behalf of the user, such as
installing non-Racket dependencies.

The setup module also acts as an entry point for a zero-trust user. If
you are suspicious of a package's contents, you can start a sandboxed
REPL in the context of a setup module The @tt{show-help} procedure is
also conventional. It gives a package author a chance to negotiate
optional setup with you.  With your consent, they can automate setup
of non-Racket resources.

@verbatim|{
$ zcpkg sandbox mavrick:pyracket:totally-works
> (show-help)
"Run (install-python3) with write permissions to these directories,
and permission to download from https://www.python.org/"
^D
$ zcpkg sandbox --sandbox-path-permissions "..." \
                --sandbox-network-permissions "..." \
                -e "(install-python3)" \
                mavrick:pyracket:totally-works
}|

If you are concerned about userspace changes even in a trustworthy
package, remember that your workspace is a valid target for
@tt{chroot}. This helps you protect the integrity of your filesystem
outside of the workspace, and helps package authors perform more
helpful tasks.


@section{Capturing a Workspace}

Once you are satisfied with the packages installed on your system, you
can generate a Racket module that installs the same packages with the
same configuration. Check this file into source control to aid
collaborators.

@verbatim|{
$ zcpkg capture > capture.rkt
}|

A capture file does @italic{not} guarentee a bit-for-bit reproduction
of a workspace, and is therefore not a complete solution for verifying
a deterministic build. It only records the @binary configuration and
packages installed, without performing any optional tasks.



@subsection{Using Installed Modules in Code}

As the name implies, zero-collection packages add no collection paths
to a Racket installation. A ZCPs modules are only generally accessible
using @racket[file]. Every installed package is given a directory
named

@racketblock[
(require (file "zcpkg-deps/example.com/pachuman/module.rkt"))
]



@section{Versioning}

Instead of major versions and company, ZCP versions use
@tech{editions} and @tech{revisions}.

A @deftech{version} is a string like @racket{legacy:12} or
@racket{minimal:newest}. Specifically, the version is an
@tech{edition} and a @tech{revision name}, separated by a colon.

An @deftech{edition} is a named sequence of revisions. A
@deftech{revision} is a state of an edition at a moment in time.

An @deftech{revision number} is a non-negative integer. Every revision
is forever assigned the next available number in an edition.

A @deftech{revision name} is a custom alias for a @tech{revision
number}. It can be a user-defined string, but strings containing all
digits, and the work @racket{newest} are reserved.

A package must have a @deftech{revision number}. When changed, a
package must increment its @deftech{revision number} if the change
uses the same @tech{edition}. If the package starts a new edition, the
@deftech{revision number} must reset to @racket[0].

The default name of an edition is @racket{draft}.

By the above rules, every package starts on version @racket{draft:0}.


@subsection{What if I want to use Semantic Versioning?}

You still can. I'd suggest making an edition for it. One of the things
an edition can define is that @tech{revision names} are semver
numbers, and the packages obey Semantic Versioning.


@subsection{Why a New Versioning Scheme?}

There are better adopted versioning schemes, so why add to the pile?
Versioning as we know it assumes that change is linear, and several
numbers are enough to communicate the nature of a change. Both
assumptions are false, yet omnipresent.

The reality is that change is @italic{conditionally} linear, and no
version value can ever communicate enough for everyone. This scheme
works a little differently:

@itemlist[
@item{An @tech{edition} references a @italic{design}.}
@item{A @tech{revision} references an @italic{implementation}.}
]


This scheme does not claim that backwards-incompatible changes can
only happen in new editions.  Breaking changes arguably occur whenever
you fix a serious error in a module-level contract, or whenever you
release a security patch. But in those cases, it might not always make
sense from an engineering (or marketing) perspective to spend time
debating which number should increment to best capture a deeply
contextual change.

Here, the rules are simpler: Did you change the implementation?  New
revision. Did you change the design? New edition. The edition does not
get a number because changes in designs are for the sake of a
particular audience (e.g. a big customer, teachers, etc).

A new edition grants an author complete creative freedom since all
assumptions about how a project works are subject to change, but a new
revision is accountable to an existing design.


@subsection{What if everyone uses this to come up with their own versioning scheme?}

It wouldn't matter. Everyone in this system has to use an edition and
a revision no matter what. The @tt{revision} part MUST support a
@tech{revision number}. @tech{Revision names} are merely
@italic{optional aliases} for @tech{revision numbers}.

Say that Sara uses Semantic Versioning, Alvin uses timestamps, and Joe
uses @italic{Joe's Awesome Versioning}. @italic{Your} dependencies can
still look like @racket{sara-edition:4}, @racket{joe-edition:921}, and
@racket{alvin-edition:0}. When one of them announces a new version that
you want, just look up the revision number and update your dependencies
accordingly.
