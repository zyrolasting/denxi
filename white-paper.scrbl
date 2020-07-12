#lang scribble/manual

@title{A Zero-Collection Package Management System}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]


@section{Abstract}

Racket projects have unique distribution challenges. This paper
presents a model that is tailored for private interests, making it
resistant to the distribution problems facing Racket's existing
package management systems. The objective of this paper is to
summarize a freemium zero-collection package management system, where
distributors do not have to worry about work interruptions on account
of another party's actions.


@section{Problem Statement}

Collections group modules in Racket. Racket packages define at least
one collection, and no two packages are allowed to create conflicting
collection paths. If packages conflict in this way and you want to use
all conflicting packages in a project, then you need to reconfigure
Racket to use both packages on the same system. One method is to use a
tethered installation, which adds a new package scope and a new place
to specify search paths and package catalogs.

While Racket installations provide tools to help users resolve package
conflicts in this way, these tools might need to be used reactively.
If you publish a software package using Racket, then it's easy for
anyone to cause package conflicts that prevent your users from
experiencing your work as intended. Specifically, users discover that
they are expected to read and apply @secref["config-file" #:doc '(lib
"scribblings/raco/raco.scrbl")].

I'll paraphrase an old joke: A physicist, an engineer, and a
mathematician were jolted awake in a burning hotel.  The engineer
doused the fire in his room with a bucket full of water. For safety,
he poured another bucket of water on the ashes.  The physicist woke
up, measured the intensity of the fire, and saw what materials were
burning. She carefully drew the necessary water from a bathtub and
surgically snuffed the fire in her room. The mathematician looked at
the fire, the bucket, and the faucet. Satisfied that there was a
solution to the fire, he went back to sleep.

The solution to Racket's difficult package management experience does
cannot be the equivalent of a bucket, a bathtub, and the sleepy
mathematician's attitude to your experience of a fire.

Businesses and active developers cannot rely on a system where
strangers and new ideas break distribution. There is an oppurtunity to
offer a solution to dependency hell with @italic{any given} Racket
installation and configuration.


@section{High-Level Solution}

The overall solution is a service that behaves like an app store for
Racket projects, combined with a package manager that safely
coordinates dependencies in a side-by-side installation model. The
system manages the conflicting interests and schedules of developers
and users, and opens the door for Racket to manage resources outside
of Racket. Both package users and package developers can install and
submit breaking changes without disrupting downstream dependencies.

In @secref{solution-details}, I define a package type and installation
model that extends ideas from established projects in this space.


@section{Competing Alternatives}

@section{Racket}

Racket has two package management systems. The first, PLaneT, attempts
to automatically distribute code such that each dependent module is
matched with dependency modules without explicit installation
commands. After complications regarding multiple versions of
dependencies in the same program, and the fact that modules implicitly
change userspace, PLaneT was deprecated in favor of @tt{raco pkg}.

@subsection{PLaneT}

PLaneT attempts to solve dependency hell at a per-module level.  A
module would request another module using the @racket[planet] form,
which may contain the name of a package, the package's owner, and
version information. Dependencies are installed automatically on use,
under the assumption that package manager use is unnecessary.

The benefit of this approach is that dependents get exactly what they
want. The drawback is that modules change userspace as a side-effect.
A single program using different versions of the same package would
suffer the consequences of conflicting change.


@subsection{@tt{raco pkg}}

Racket installations have scoping rules for packages. A package's
scope is a function of the installation's configuration, and how
Racket is launched. The packages you install contribute modules to
some collection, and the Racket module resolver will search for a
collection given a collection path. The benefit to this approach is
clean, path-like identifiers for use in @racket[require].

The drawback is that two packages can submit conflicting collection
paths without the authors' (or author's) knowledge, forcing the user
to reconfigure the Racket installation and/or package manager to run
their programs such that they use the correct dependencies. The
resulting experience becomes "How do I configure and launch Racket to
make this set of dependencies work"? In that sense, @tt{raco pkg}
places the burden for solving dependency hell on the user.

The Racket default catalog does not keep copies of old versions of
your packages, so you can only replace your package on the catalog if
you understand that doing so will break downstream dependencies (often
times on the catalog itself). But, if you publish the package under
the name @tt{allstar2}, it will still conflict with the files in the
other published package. You and your users have to use the
@italic{collection name} @tt{allstar2}.  Communicating this may be
confusing to users if the collection @tt{allstar2} comes in a package
with a @tt{1.*} version number.

The side-effects of these conflicts include broken links to your
documentation and an increased burden for your users to manage
upstream dependencies.

Your code, in effect, shares a single directory with all other code
submitted by publishers. Make a package on the Racket catalog at
@hyperlink["https://pkgs.racket-lang.org"]{pkgs.racket-lang.org}, and
give it a unique name. Tell the catalog to pull the source code from
the same source code repository owned by your target's author. In a
few hours, the online docs for the other person's package will be
accessible from the catalog, but the links to that package's docs will
be broken on @hyperlink["docs.racket-lang.org"]{docs.racket-lang.org}.
The new package conflicts with the original author's package,
preventing either from working as intended in the same Racket
installation.

Corollary to this: You can generate a Racket package that impacts all
online third-party documentation in this way. You can also "not
realize" that you used a directory or file name that conflicts with a
less-adopted competitor, such that a user cannot easily have both
packages installed on their system.

In discussing the implications with the Racket community I learned
that the developers did not find that these problems came up enough in
practice to warrant action. Jay McCarthy claimed that his @tt{raco
pkg} captured the "ethos" of Racket. Ultimately it is up to the
developers and users to adapt their code to the package manager and
its ideas of what conflict means for an installation.


@subsection{NPMv3}

NPMv3 leverages Node.js' module resolution algorithm to install
packages around the user's system. Given
@tt{require('package/path/to/module.js')}, Node.js will walk towards
the root directory in search of a
@tt{node_modules/package/path/to/module.js}. The search fails if the
resolver reaches the root directory.

The benefit of this approach is that the filesystem heirarchy acts as
a scoping mechanism: NPMv3 puts shared dependencies in a @tt{node_modules}
directory closer to the root, and any conflicting dependencies are
installed in a @tt{node_modules} directory closer to the relevant
dependents.

A drawback to this approach is that packages do not shadow other
packages.  If a @tt{require('pkg/m.js')} fails to find a
@tt{node_modules/pkg/m.js}, the search will continue and possibly match
a @tt{../../node_modules/pkg/m.js} in a conflicting version of @tt{pkg}.
This scenario can arise in projects where users are unaware of
module-level changes in a package during an update, when the
dependency has conflicting versions.


@subsection{Python/Pip}

Python's package management system has many tools, but modules
provided in conflicting packages are found in a lexographical ordering
of package names.


@section[#:tag "solution-details"]{Solution Details}

Racket allows you to define a single-collection package or a
multi-collection package. The close the gap in casework, a
@deftech{zero-collection package} (@deftech{ZCP}) is a package that
defines no collection. On disk, a ZCP is just a directory with a
tailored @tt{info.rkt} file. That @tt{info.rkt} follows the semantics
of this document and any other reference material.

ZCPs impose no conventions on how they are used, which allows
developers to define distribution rules independently of the PLT. The
package manager, @tt{zcpkg}, offers one such set of rules. It handles
the same concerns addressed by @tt{raco setup} and @tt{raco pkg},
because ZCPs have no collections to target. While ZCPs cannot be
referenced directly by a collection path, it can be referenced within
an existing collection.

ZCPs may or may not be checked into other developers' source code.  In
the event a ZCP is not present in a developer's project, it must be
installed from a source like any other package.

Another benefit of this approach is that a ZCP can orchastrate
non-Racket dependencies in userspace. That way you no longer have to
tell your users to run off and install Python, Docker, or some other
application. You can do it for them if needed.

@tt{zcpkg} gives the user tools to verify remote artifacts and to
verify that installations are reproducible. It may operate offline to
install ZCPs from directories or archives on the user's filesystem.

@tt{zcpkg} can also start a service that maps URNs to ZCPs. Users can
reserve their own identities in a public namespace, but package names
are scoped under their identity. This is to address a budding demand
in the Racket community for personal catalogs.

Once a ZCP is published, it is assigned a permanent revision number in
that scheme that can never be replaced.  A ZCP may be replaced during
a grace period to address emergencies, but the artifact will be
permanently set afterwards.

URNs may include a @deftech{version}, which consists of an
@tech{edition} and a @tech{revision}.

An @deftech{edition} is a named sequence of revisions. A
@deftech{revision} is a state of an edition at a moment in time.

An @deftech{revision number} is a non-negative integer. Every new
revision is forever assigned the next unused number in an edition.

A @deftech{revision name} is a custom alias for a @tech{revision
number}. It can be @racket{oldest} (an implicit alias for @racket[0]),
@tt{newest} (an implicit alias for the last used revision number), or
a user-defined string.

A package MUST have a @deftech{revision number}. When changed, a
package MUST increment its @deftech{revision number} if the change
uses the same @tech{edition}. If the package starts a new edition, the
@deftech{revision number} MUST be @racket[0].

The default name of an edition is @racket{draft}.

By the above rules, every package starts on version @racket{draft:0}.
