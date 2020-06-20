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

If you publish a software package using Racket, then it's easy for
anyone to cause package conflicts that prevent your users from
experiencing your work as intended. For example: A stranger can break
links to @italic{your} online documentation by naming a file in
@italic{their} project a certain way. That same stranger can also "not
realize" that they used a directory name in their source code that
prevents users from installing your application or
library. Additionally, teams are not able to pin versions, making it
harder to support users over time. Examples of these problems are
listed in @secref{...}.

There are already solutions to package conflicts in Racket, but the
solutions are evidence of the problem: One can interrupt distribution
of your work at any time, intentionally or unintentionally, and you
can only @italic{react} to that.

In general, there are no preventive measures you can take to protect
your project from the effects of package conflicts. But the way Racket
allows a user to handle conflicts requires detailed knowledge of how
Racket starts that other package ecosystems (e.g. NPM) do not ask of
developers.

This presents an oppurtunity: Give private developers and publishers a
way to distribute their work that is far less likely to interrupt
them, and won't require a detailed understanding of how Racket
works. Users who experience package conflicts can resolve them with
ease, and maintainers can get the peace of mind that distribution
won't complicate their work.


@section{High-Level Solution}

The overall solution is a service that behaves like an app store for
Racket projects, combined with a client that safely coordinates
dependencies with a small learning curve. The system manages the
conflicting interests and schedules of developers and users, and opens
the door for Racket to manage resources outside of Racket.

In the solution details, I define a package type and process that
borrow from Racket's prior work in this space and adds valuable
conveniences for users and developers. I address problems in the
prior work and the alternative design choices that protect users from
errant behavior.


@section{Competing Alternatives}

Racket has two package management systems. The first, PLaneT, attempts
to automatically distribute code such that each dependent module is
matched with dependency modules without explicit installation
commands. After complications regarding multiple versions of
dependencies in the same program, and the fact that modules implicitly
change userspace, PLaneT was deprecated in favor of @tt{raco pkg}.

The Racket default catalog does not keep copies of old versions of
your packages, so you cannot replace your package on the catalog
outright. But, if you publish the package under the name
@tt{allstar2}, it will still conflict with the files in the other
published package. You and your users have to use the
@italic{collection name} @tt{allstar2}.  Communicating this may be
confusing to users if the collection @tt{allstar2} comes in a package
with a @tt{1.*} version number.

The side-effects of these conflicts include broken links to your
documentation and an increased burden for your users to manage
upstream dependencies.

Your code, in effect, shares a single directory with all other code
submitted by publishers. Make a package on the Racket catalog at
pkgs.racket-lang.org, and give it a unique name. Tell the catalog to
pull the source code from the same GitHub repository owned by your
target's author. In a few hours, the online docs for the other
person's package will be accessible from the catalog, but the links to
that package's docs will be broken on docs.racket-lang.org.  The new
package conflicts with the original author's package, preventing
either from working as intended in the same Racket installation.

Corollary to this: You can generate a Racket package that impacts all
online third-party documentation in this way. You can also "not
realize" that you used a directory or file name that conflicts with a
less-adopted competitor, such that a user cannot easily have both
packages installed on their system.

In discussing the implications with the Racket community I learned
that the developers did not find that these problems came up enough in
practice to warrant preventive measures. The package manager as it
stood captured the "ethos" of Racket, and it was up to the developers
and users to adapt their code to the package manager and its ideas
of what conflict means for an installation.

I'll paraphrase an old joke: A physicist, an engineer, and a
mathematician were jolted awake in a burning hotel.  The engineer
doused the fire in his room with a bucket full of water. For safety,
he poured another bucket of water on the ashes.  The physicist woke
up, measured the intensity of the fire, and saw what materials were
burning. She carefully drew the necessary water from a bathtub and
surgically snuffed the fire in her room. The mathematician looked at
the fire, the bucket, and the faucet. Satisfied that there was a
solution to the fire, he went back to sleep.

This paper does not offer the equivalent of a bucket, a bathtub, and
the sleepy mathematician's attitude to your experience of a
fire. Businesses and active developers cannot rely on a system where
strangers and new ideas break distribution. There needs to be a way to
send Racket projects to the world easily and reliably.


@section{High-Level Solution}

@section{Zero-Collection Packages}

Racket allows you to define a single-collection package or a
multi-collection package. The close the gap in casework, a
@deftech{zero-collection package} (@deftech{ZCP}) is a package that
defines no collection. On disk, a ZCP is just a directory with a
metadata file.

The benefit of ZCPs is that they impose no conventions on how they are
used, which allows developers to make their own idioms for sharing
code, independently of the PLT. The idioms and user code determine how
vulnerable ZCP are to conflicts and collateral damage.

ZCPs cannot be directly targeted by @tt{raco setup} or a collection
paths, although a collection's @tt{info.rkt} file may decide to
include it as part of the collection's contents.

ZCPs do not have to be checked into other developers' source code.
They must be installed from a source like other packages. To aid use
of non-Racket dependencies in userspace, ZCPs may include an installer
to apply the work @tt{raco setup} does to the contents of the package.
Another benefit of this approach is that a ZCP can orchastrate
non-Racket dependencies in userspace. That way you no longer have
to tell your users to run off and install Python, Docker, or some
other application. You can do it for them if needed.

To address changes in operating conditions, this project comes with a
client, a server, and a library underlying the two.


@subsection{The Client}

The client is a package manager @tt{raco zcpkg} that, like @tt{raco
pkg}, prepares packages and their dependencies for use. The difference
is that the package manager only targets ZCPs, leaving @tt{raco pkg}
to work on packages with collections.

Unlike @tt{raco pkg}, @tt{zcpkg} does not use @tt{raco setup} because
there are no collections to target. It instead mediates an exchange
between the user and an installer in the ZCP (if any). To mitigate the
risk of malicious code, @tt{zcpkg} checks digital signatures and
digests.

@tt{zcpkg} generates lock files based on installed ZCPs. If a user
installs dependencies that disagree with the lock file, then that user
will be alerted to all discrepencies. This mitigates "works on my
machine" problems. A lock file @italic{may} store per-platform
integrity information after an installer runs to help a user verify if
a build is deterministic.

While the user can leverage ZCP as part of an existing collection, a
@tech{ZCP}'s installer should do what @tt{raco setup} does for its own
contents.  The installer runs after all dependencies of a ZCP are
installed, which means you can use ZCPs in an installer.

The client may operate offline. In that case it can install ZCPs from
directories or archive files on the user's filesystem.


@subsection{The Server}

The server organizes ZCPs under universally-unique addresses bound to
the identity of a publisher. Publishers compete over use of their own
identities in a public namespace. Package authors should only compete
with collaborators, not the entire Racket community, over names like
@tt{framework} or @tt{html}.

The server also uses a generalized versioning scheme to let users
define what they want. Once a ZCP is published, it is assigned a
permanent revision number in that scheme that can never be replaced.
A ZCP may be @italic{retracted} in emergency situations such as a
secret leak, but no other ZCP will ever again have that revision
number on the service.

Altogether, a user can request an exact artifact.

@subsection{The Library}

The library defines operations regarding ZCPs such that a user can
work with them.

Once installed, a user may @racket[require] modules in an ZCP.
Scoping rules for ZCPs in the file system prevents accidental use of
modules in a package in a broader scope.


@section{Solution Details}

To install a package, run @tt{raco zcpkg install <package-source>}.

A @deftech{package source} is a path to a directory, an
@tech{artifact}, a GitHub repository URL, or a URL where the response
is an @tech{artifact}. When you install a package from a @tech{package
source}, the package manager will try to convert the @tech{package
source} to a @tech{package} in the @tech{install directory}. The
install directory will be in the current working directory where you
ran the command.

An install command can request a dependency in the abstract, such that
you don't know the exact version installed. You can create a
@deftech{lock file} using the current contents of the @deftech{install
directory} to ensure that future installations produce the same
content.

@verbatim|{
$ raco zcpkg lock
}|

You should check all lock files into version control so that your team
can reproduce the same dependency tree. You can verify that a
dependency tree matches the lock file before working.

@verbatim|{
$ raco zcpkg verify
}|

A ZCP's @tt{info.rkt} may name an @deftech{installer}, which is a
Racket module that integrates the package with the surrounding
system. The package manager will run the installer under a
that offers zero trust to remote resources. See @secref{security}.

@section{Versioning}

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
