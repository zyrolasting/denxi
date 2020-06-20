#lang scribble/manual
@require[@for-label[racket/base] zcpkg/path]

@title{A Zero-Collection Package Management System}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

This project offers a means to share Racket applications and libraries
that improves on @tt{raco pkg}.

Benefits:

@itemlist[

@item{You may define your own standards for distribution.}

@item{Lock files ensure reproducible dependency installations.}

@item{Package conflicts are easy for users to resolve.}

@item{Digital signature checking.}

@item{You have your catalog, therefore your own namespace for packages.}

@item{Authors have more control over the installation process for their own packages.}

]

@section{Scope}

This is a reference for the @tt{zcpkg} package manager and related
libraries.

Interpret the capitalized words MAY, MUST, and SHOULD according to
@hyperlink["https://tools.ietf.org/html/rfc8174"]{RFC 8174 (IETF)}.

@section{Model}

A @deftech{zero-collection package} (or @deftech{ZCP}) is a package
that does not define a collection. Outside of added @tt{info.rkt}
configuration, the ZCP is an inert directory. A user invokes @tt{raco
zcpkg} to install, distribute, and develop ZCPs. In this context, ZCPs
are only valid targets for @tt{raco setup} and the module resolver
through an existing collection.

The reason for the additional workflow is to allow users to define
their own idioms and conventions for packages in an unmodified Racket
installation. This makes it possible to keep around packages that
would otherwise conflict by working outside of the collection
namespace.

To address these changes in operating conditions, this project comes
with a client, a server, and a library underlying the two.

The server organizes @tech{version}ed ZCPs as @tech{artifacts} under
@tech{publishers}' @tech{brands}, such that a publisher can distribute
backwards-incompatible releases with no technical impact on marketing
efforts, users, or downstream dependencies.  Per-publisher namespacing
means that as a package authors do not compete with the entire
Racket community over names like @tt{framework} or @tt{html}.

ZCPs may run code under a security policy to modify userspace, such as
fetching and building non-Racket dependencies. This is to enable
installation of non-Racket dependencies. A user must explicitly
consent to any such modifications, either proactively or
interactively.

Once installed, a user may @racket[require] modules in a ZCP.
Scoping rules for ZCPs in the file system prevents accidental
use of modules in a package in a broader scope.

Overall, the system behaves like an "app store" for Racket projects
that manages the conflicting interests of developers and users, and
opens the door for Racket to coordinate the resources in other
ecosystems for the sake of a cohesive application.


@section{The Client: @tt{raco zcpkg}}

@(define depdir @(tt @DEPENDENCY-DIRNAME))

To install a package, run @tt{raco zcpkg install <package-source>}.

A @tech{package source} for @tt{raco zcpkg} means the same as it does
for @tt{raco pkg}. The difference is that @tt{zcpkg} will install a
package in a @depdir subdirectory right in front of you.

You can create a @deftech{lock file} using the current contents of a
@depdir to help others install the exact same thing that you do later.

@verbatim|{
$ raco zcpkg lock
}|

You should check all lock files into version control so that your team
can reproduce the same dependency tree. You can verify that a
dependency tree matches the lock file before working.

@verbatim|{
$ raco zcpkg verify
}|

An ZCP's @tt{info.rkt} may name an @deftech{installer} that changes
the userspace under a security policy. Developers can use this to
install non-Racket dependencies with the user's consent.


@section{Service}

@tt{zcpkg} can work entirely offline, but a server is available at
@SERVICE-URL to host ZCPs. ZCPs use a versioning scheme based on
@tech{editions} and @tech{revisions} (Optional reading: @secref{why-version}).

@subsection{Versioning}

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


@subsection{Publishing}

An @deftech{artifact} is a @tt{.tgz} file that contains @italic{exactly one} @tech{package}.

A @deftech{brand} is a named group of @tech{artifacts}.

A @deftech{catalog} is a group of @tech{brands}.

A @deftech{publisher} is a person or organization with a @tech{catalog}.

To publish a package, a @tech{publisher} places an @tech{artifact}
under a @tech{brand}. Once a user discovers the brand they want, they
request an @tech{artifact} from that brand by @tech{version}.

A user retrieves an artifact by publisher, brand, and version.

When a publisher shares a package, the package must be filed organized
under a @tech{project}. The package's @tech{edition} and
@tech{revision} further qualify how it can be later retrieved.

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
