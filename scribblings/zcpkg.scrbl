#lang scribble/manual
@require[@for-label[racket/base]
         "../workspace.rkt"]

@title{A Zero-Collection Package Management System}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

This project offers a means of sharing Racket applications and
libraries to the following ends:

@itemlist[

@item{You may define your own standards for distribution.}

@item{You can reproduce the same dependency installations across devices.}

@item{You can resolve package conflicts easily, without in-depth knowledge of Racket.}

@item{You can authenticate packages before using them.}

@item{You can automate installation of Racket and non-Racket dependencies.}

@item{You can run automated installations in a sandbox.}

@item{You can publish packages to your own catalog.}

@item{You can write packages that contribute to custom operating systems.}

]

@section{Scope}

This is a reference for the @tt{raco zcpkg} package manager and related
libraries.

@section{Installation}

@tt{zcpkg} is available as a single-collection Racket package, so you
can install it using @tt{raco pkg install zcpkg}.  You will also need
the binaries for OpenSSL, starting from version 0.9.8, available in
your search paths for the sake of the SHA-2 algorithm suite.

However, @tt{zcpkg} is also available as a @tech{zero-collection
package} (see @secref{model}). In this latter case, the package
manager can install any version of itself and make sure the correct
OpenSSL version is available (See @secref{model}).

If you are already a user, you can install a @tech{workspace}-specific
@tt{zcpkg} using @tt{raco zcpkg install zcpkg}.


@section[#:tag "model"]{Summary of Model}

A @deftech{zero-collection package} (or @deftech{ZCP}) is a package
that does not define a collection when installed. Besides having an
@tt{info.rkt} file, a ZCP is a dumb directory.

To clarify, whether a package has collections or not is a designation
that matters to the user.  A directory that defines a well-formed
Racket package in the context of @tt{raco pkg} can still be used as a
zero-collection package in the context of this project.

As the name implies, a zero-collection package provides no valid
target for @tt{raco setup} or collection paths, unless it is leveraged
through an existing collection.

To address changes in how installation works, this project comes
with a client, a server, and libraries underlying the two. The overall
system behaves like an "app store" for Racket projects that manages
the conflicting interests of developers and users, and opens the door
for Racket to coordinate the resources in other ecosystems for the
sake of more aspirational projects.


@subsection{Client}

The client, @tt{raco zcpkg}, is a package manager that operates on
Racket packages as if they define no collection.

@bold{Document convention:} When there is no room for confusion with
@tt{raco pkg}, I will refer to @tt{raco zcpkg} as "the package
manager."

Like @tt{raco pkg}, @tt{raco zcpkg} uses @tt{info.rkt} to decide how
to handle the contents of a directory. The difference is that the
package is responsible for its own integration.

Zero-collection packages conflict on filesystem paths, but not
collection paths. Even so, each ZCP is installed to a location you
specify. This way conflicts only occur if you install the same package
to the same place.

The package manager supports integrity-checking and authentication for
remote artifacts, so that a user can mitigate the risk of executing
other people's code at their chosen privilege level.  The package
itself is allowed to operate in its own source directory, and a
directory meant to follow the Filesystem Heirarchy Standard (be it the
actual root directory of the system, or a jailed variant). The
intended experience is that a Racket package can, under the runtime of
a given Racket installation and any security considerations,
contribute a few libraries or construct an entire other operating
system running a different version of Racket.


@subsection{Server}

The server saves all contributed ZCPs in per-account catalogs.  Each
account is owned by a @deftech{distributor}. A distributor can submit
backwards-incompatible releases with no technical impact on marketing
efforts, users, or downstream dependencies.  Having their own catalog
means that a distributor does not need to compete compete with the
entire Racket community over names like @tt{framework} or @tt{html}.

ZCPs may run code under a security policy to modify userspace, such as
fetching and building non-Racket dependencies. This is to enable
installation of non-Racket dependencies. A user must explicitly
consent to any such modifications, either proactively or
interactively.

Once installed, a user may @racket[require] modules in a ZCP.
Scoping rules for ZCPs in the file system prevents accidental
use of modules in a package in a broader scope.

@(define depdir @(tt @CONVENTIONAL_WORKSPACE_NAME))


@section{How to Read this Manual}

Like all package management systems, @tt{zcpkg} has rules about how it
moves files around on your disk and runs code. This section covers
those rules and defines useful document conventions.


@subsection{Workspace directory}

The package manager starts life by searching for a special
@deftech{workspace directory} named @|depdir|. When there is no room for
confusion, I will sometimes refer to this directory and/or its
contents as a @deftech{workspace}. The package manager will first
check to see if @depdir is a subdirectory of
@racket[(current-directory)].  Failing that, it will check every
parent directory in a walk towards a root directory. If @depdir does
not exist, then the package manager will assume that it needs to
create one in @racket[(current-directory)].

@bold{Document convention:} When I refer to a path using a leading
@litchar{:}, as in @litchar{:/usr/local}, I am referring to a path
relative to the @depdir used by the package manager.

@depdir is meant to comply with Version 3.0 of the
@hyperlink["https://refspecs.linuxfoundation.org/FHS_3.0/fhs/index.html"]{Filesystem
Heirarchy Standard (FHS)}. The package manager uses it to store
configuration information, installed packages, and cached downloads.

Doing it this way yields some properties:

@itemlist[

@item{@italic{All} information used by the system is in this
directory, so there isn't any mystery about where caches or packages
are.}

@item{Any one of your projects can have its own @depdir, and therefore
its own configuration and dependencies. Each @depdir is fully isolated
unless you go to your own lengths to link them together.}

@item{The package manager can use Racket packages to construct an
operating system or jailed application.}

]


@subsection{Configuration}

Both the package manager and service use a configuration space derived
from English labels of individual settings.

Let a hypothetical setting be “Use Widget”, which is a boolean.

In this case, the value of this setting is the last item in this list
to define a value:

@itemlist[#:style 'ordered

@item{A hard-coded default.}

@item{A @racket[read]-able literal in @litchar{:/etc/zcpkg/ZCPKG_USE_WIDGET}.}

@item{An environment variable named @tt{ZCPKG_USE_WIDGET}.}

@item{A command-line flag named @tt{--use-widget}. If the value is a
boolean, the flag itself will suffice. Otherwise it is a string
containing a @racket[read]-able value as a literal.}
]

So, in the case of file names and envvars, all sequences of at least
one non-alphanumeric character in the setting name is replaced with a
single underscore. The transformed name is set to use to all uppercase
letters and a @tt{ZCPKG_} prefix.

In the case of command-line flags, the resulting name has no prefix
and hyphens take the place of underscores. Abbreviated command-line
flags are issued on a case-by-case basis.

@bold{Document convention:} Documented settings use links to
documented parameters using their capitalized
name. e.g. @racket[ZCPKG_INSTALLER_MEMORY_LIMIT_MB]. These links will
appear even in interface definitions that would format the name
differently.



@section{Installers}

An ZCP's @tt{info.rkt} may name an @deftech{installer} that changes
the userspace under a security policy. Developers can use this to
install non-Racket dependencies with the user's consent.

By default, the installer runs in a restricted @racket['racket/base]
sandbox as in @racket[make-module-evaluator]. The package manager
configuration adjusts the sandbox according to
@secref["Customizing_Evaluators" #:doc '(lib
"scribblings/reference/reference.scrbl")].

The installer will only run under the following conditions:

@itemlist[

@item{All dependencies for the installer's package in the @tech{workspace} are fulfilled.}

@item{The @tt{info.rkt} file declares a expected Racket version that matches @racket[(version)].}

@item{The user consents to running the installer.}

]

@subsection{Security Implications}

For illustration purposes, this is how stupid people (or smart people
studying maximum-trust conditions) would use the package manager.

@verbatim|{
$ sudo raco pkg \
  --trust-unsigned \
  --trust-signature-mismatch \
  --workspace='"/"' \
  --installer-path-permissions='((write) "/")' \
  install \
  ram-upgrade
}|

I trust that you know the risks behind @tt{sudo}, but even without it,
you should not run an @tech{installer} from the Internet unless you
know what it is going to do. @bold{An installer is untrusted code.}

The package manager uses digital signature checking, integrity
checking, @racket[racket/sandbox], and asking you if you are really
super-dooper sure to @italic{mitigate}, not eliminate, the risks. It
is up to you to pass control to installers for userspace modifications
after you follow sound security practices.

Assume that your operating system, Racket installation, and package
manager are all uncompromised and in working order. Also assume that
@tt{ram-upgrade} is evil and out to get you. Let's talk about why the
above command allows that.

@verbatim|{
  --trust-unsigned \
  --trust-signature-mismatch
}|

This shuts off digital signature checking. You only do this if
you trust that a package really came from the author you expect.

@verbatim|{
  --workspace='"/"'
}|

While it is not the default, you can configure the @tech{workspace
directory} to be the root directory. This means that installers can
have system-wide impact unless you use @tt{chroot} or a container to
jail the process.

@verbatim|{
  --installer-path-permissions='((read write execute) "/")'
}|

This breaks the sandbox's file I/O protections.

An @tech{installer} can use this to elevate the privileges of
subsequent installations if you set
@racket[ZCPKG_INSTALLER_PATH_PERMISSIONS] to a value that is too
permissive. Specifically, privilege escalation is possible if you give
the installer write access to @litchar{:/etc/zcpkg} or the source code
of anything that plays a part in running installers. Setting the
permissions to the workspace itself gives an installer free reign to
modify not just the configuration, but everything in the
@tech{workspace}.


@section{Service}

@tt{zcpkg} can work entirely offline, but a server is available
to host ZCPs on behalf of a @deftech{distributor}.  A
distributor can be a publisher, the actual author or a package, or
some other party. The service only trusts that an authenticated
distributor has the right to distribute the packages they submit.
It will leave the legalities for the mortals.

A package is stored as a @tt{.tgz} artifact with a
distributor-asserted SHA-384 or SHA-512 digest and related RSA
signature. When the client downloads an artifact, the user is alerted
to a mismatch in the digest and/or signature. A @tech{distributor}
must submit an artifact with appropriate metadata.


@section{Versioning}

Instead of major versions and company, package versions use
@tech{editions} and @tech{revisions} (Optional reading:
@secref{why-version}).

A @deftech{version} is a string like @racket{legacy:12} or
@racket{minimal:newest}. Specifically, the version is an
@tech{edition} and a @tech{revision name}, separated by a colon.

An @deftech{edition} is a named sequence of revisions. A
@deftech{revision} is a state of an edition at a moment in time.

An @deftech{revision number} is a non-negative integer. Every revision
is forever assigned the next available number in an edition.

A @deftech{revision name} is a custom alias for a @tech{revision
number}. It can be @racket['oldest], @racket['newest], or a string.

A package MUST have a @deftech{revision number}. When changed, a
package MUST increment its @deftech{revision number} if the change
uses the same @tech{edition}. If the package starts a new edition, the
@deftech{revision number} MUST be @racket[0].

The default name of an edition is @racket{draft}.

By the above rules, every package starts on version @racket{draft:0}.



@section{Usage Guide}

@subsection{Install a Package}

@verbatim|{
$ raco zcpkg install <package-source> ...
}|

@margin-note{Why less support for package scopes? The honest reason is to
reduce the scope of work for the first release of this project. Parity
with @secref["concept:source" #:doc '(lib
"pkg/scribblings/pkg.scrbl")] is a goal, provided that checksums
follow the integrity-checking approach of this project.}
A @deftech{package source} for @tt{raco zcpkg} is @italic{not} the
same as a package source for @tt{raco pkg}. The only supported package
sources are local directories, @tt{.tgz} archives, and
@tech{dependency queries}.

In the event the resource comes from a remote location, the package
manager will warn of integrity violations or missing/invalid
signatures.  The command line interface has options to clarify your
level of trust and patience. By default, the package manager
will interactively handle dependencies and untrusted code.

@subsection{Uninstall a Package}

@verbatim|{
$ raco zcpkg uninstall package-name
}|

Runs the uninstaller in the named package, and then deletes the
package's files. Any isolated dependencies are also automatically
uninstalled unless you specify otherwise.


@subsection{Update a Package}

@verbatim|{
$ raco zcpkg update package-name <package-source>
}|

By default, updating a package uninstalls a package and reinstalls the
version from the given package source. The package found from the
package source must differ only in version from the named package.


@subsection{Using Installed Modules in Code}

Because zero-collection packages do not cause a Racket installation to
define collections, there are no collection paths to any particular
module of a ZCP. The paths are therefore long and tedious, and
different abbreviations are neccessary.

Given the conventions established, we'll use include paths a la C projects.

The least abbreviated @racket[require] form is @racket[zcfile].

@racketmod[racket/base

(require zcpkg
         (zcfile "/usr/local/.../module.rkt")
         (zcfile "/usr/.../other.rkt"))]

The @racket[zcfile] uses an “absolute” path is actually relative to
the @tech{workspace} to find a file.

Next, A zero-collection package can search for module dependencies via
@racket[ZCPKG_REQUIRE_SEARCH_PATHS] using @racket[zcinc].

@racketmod[racket/base

(require zcpkg
         (zcinc "[...]/module.rkt")
         (zcinc "[...]/other.rkt"))]

Since the packages do not deal in collections, you can either define
your own collections, reprovide bindings in a module closer to your code,
or define a custom abbreviation using @racket[make-zcpkg-require-transformer].


@subsection{Capture and Restore Workspaces}

You can create an @deftech{capture file} using the current contents of
a @depdir to help others measure the exact difference between their
@tech{workspace directory} and yours. The capture file contains the
entire package manager configuration, integrity information for all
files, and the exact order of commands executed to produce the
directory.

@verbatim|{
# Record integrity information
$ raco zcpkg capture > cap
$ git add integrity

# On other person's machine.
$ git clone ...
$ raco zcpkg install ...
$ raco zcpkg diff cap
}|

The integrity file is also input for the @tt{restore} command.
The @tt{restore} command first deletes @depdir and attempts
to recreate it such that @tt{raco zcpkg diff} has no output.

@verbatim|{
$ raco zcpkg restore cap
}|

@margin-note{Since the capture file also contains the package manager
configuration, it is possible to create a capture file that verifies
(optional) per-platform integrity information. This applies to files
created by the packages themselves, such as compiled
artifacts. Leverage this if you care about deterministic builds.}

You should check all capture files into version control so that your
team can reproduce the same workspace. You can verify
workspace integrity using the @tt{verify} command.

@verbatim|{
$ raco zcpkg verify cap
}|


@subsection{Resolving ZCP Modules}

Because ZCPs are distributed around a user's filesystem, the user must
resolve modules with scoping rules derived from directory conventions.



@defform[(zcpkg zcpath ...)]{
A @racket[require] form that finds a module inside an
@tech{zero-collection package}, where each @racket[zcpath] is
a relative path string to a module in some @depdir directory.

For example, let an @racket[ap-path] be
@racket{publisher-name/package-name/path/to/module.rkt},
@racket[zcpkg] will walk towards the root directory in search of the
module.

A packages of the same name shadow each other's

This is important to emphasize: The search is first for the publisher
and package, then the module. This means that @racket[zcpkg] will NOT
continue searching for a module if the package is found.

The possible dependency directories are all found by walking the
filesystem towards the root directory, starting from:

@itemlist[
@item{The directory in which @racket[zcpkg] appears in source; or}
@item{@racket[(current-directory)], if the source location is unavailable.}
]

If the search reaches the root directory, then the search fails and
@racket[zcpkg] will raise an error. A dependency directory may hold a
link file that contains a path to another directory on the system. If
a dependency directory does not contain the target module, then the
search will continue at the path designated in the link file instead
of the next parent directory.

If a module is not found in a ZCP, but the ZCP directory exists, then
the search concludes immediately. This allows a ZCP to “rebind” other
ZCPs of the same name in other dependency directories.

@bold{Corollary:} A link provides scoping rules for search. For
example, it can connect “local” dependencies to “global” dependencies.

@bold{Corollary:} A link that points to the root directory will
terminate the search. This in effect makes a dependency directory
“top-level”.
}


@section{Why Brands?}

Grouping artifacts in a brand means that you can market different
editions or revisions of packages under an unchanging name and set of
assumptions.


@section{Tradeoffs}

Packages are fully decoupled from Racket installations. Users can
install and use packages that would otherwise conflict, but cannot use
collection paths to resolve modules in those packages.


@itemlist[

@item{Packages do not populate collections. Modules are resolved using the @racket[zcpkg] require subform.}
@item{All packages use the same versioning scheme.}
@item{A vendor can organize their files however they wish without conflicting with other packages.}
@item{A user can install packages in any directory.}
@item{A user can create and use lockfiles to reproduce installations on other systems.}
@item{A user can express abstract and concrete dependencies.}
]



@section[#:tag "security"]{Security}

Every package is installed under a security guard and custodian that
gates filesystem and network access behind user consent, and issues
quotas for memory use.

By default, a package is untrusted and will be unable to perform any
I/O. If the package came in an unsigned @tech{artifact}, the user will
be urgently cautioned against installing the package unless they are
aware of its activity.

@section{Problem Statements}

@subsection{Optional Reading: Why a New Versioning Scheme?}

There are better adopted versioning schemes, so why add to the pile?

Versioning as we know it assumes that change is linear, and one or two
version values are enough to communicate the nature of a change. Both
assumptions are false, yet omnipresent.

The reality is that change is @italic{conditionally} linear, and no
version value can ever communicate enough for everyone. This scheme
works a little differently:

@itemlist[
@item{An @tech{edition} defines broad assumptions about @italic{how} @tech{packages} work.}
@item{A @tech{revision} defines adjustments within an edition.}
]

A new edition grants an author complete creative freedom since all
assumptions are subject to change, but a new revision must consider
users tracking the progress in an edition.

This scheme does not claim that backwards-incompatible changes can
only happen in new editions. It claims that if there is a
backwards-incompatible change in a revision, it was made in
consideration of the users' interests (e.g. security patches, feedback
during prototyping).


@subsubsection{What if I want to use Semantic Versioning?}

You still can. I'd suggest making an edition for it. One of the things
an edition can define is that @tech{revision names} are semver
numbers, and the packages obey Semantic Versioning.

@subsubsection{But what if everyone has their own versioning scheme? I
don't want to declare dependencies with a mix of semver, dates, and
whatever incantation people make up.}

Everyone in this system has to follow the same version format no
matter what: @racket{edition:revision}. The @tt{revision} part MUST
support a @tech{revision number}. @tech{Revision names} are merely
@italic{optional aliases} for @tech{revision numbers}.

Say that Sara uses Semantic Versioning, Alvin uses timestamps, and Joe
uses @italic{Joe's Awesome Versioning}. @italic{Your} dependencies can
still look like @racket{sara-edition:4}, @racket{joe-edition:921}, and
@racket{alvin-edition:0}. When one of them announces a new version that
you want, just look up the revision number and update your dependencies
accordingly.

@section{@tt{info.rkt} file format}

You can use your existing @tt{info.rkt} files to communicate with @tt{zcpkg}.
It might be best to separate data for different package managers for legibility.
