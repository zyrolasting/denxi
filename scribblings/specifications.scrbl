#lang scribble/base

@title{A Comparison of Package Scoping Rules}

Package managers need to avoid creating a dependency hell.

Since packages might not come as independent executables with
everything it needs built-in, we'll need to define scoping rules in
terms of filesystem paths, and give users a way to reason about those
rules.

@section{Racket}

Racket has two package management systems. Each has their own idioms
and conventions.


@subsection{PLaneT}

PLaneT attempts to solve dependency hell at a per-module level.  A
module would request another module using the @racket[planet] form,
which may contain the name of a package, the package's owner, and
version information. Dependencies are installed automatically on use,
under the assumption that package manager use is unnecessary.

The benefit of this approach is that dependents get exactly what they
want. The drawback is that dependencies can change userspace as a
side-effect, and any overlap in their functionality can cause
problems.


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


@section{NPMv3}

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
@{node_modules/pkg/m.js}, the search will continue and possibly match
a @{../../node_modules/pkg/m.js} in a conflicting version of @tt{pkg}.
This scenario can arise in projects where users are unaware of
module-level changes in a package during an update, when the
dependency has conflicting versions.


@section{Python/Pip}

Python's package management system has many tools, but modules
are found in a lexographical ordering of package names [Bogdanp].
I'd want to give users a way to avoid that.


@section{Designated Approach}

I'm trying a solution that borrows from several approaches.

@tt{zcpkg} defines a filesystem convention where one @tt{zcpkgs}
directory sits in a user-specified location. @tt{zcpkgs} contains all
installed versions of any dependency, such that the path to any module
is unique.

The benefit of this approach is that installing conflicting
dependencies is trivial, and all operations on dependencies has a
predictable impact.  As an aside, if @tt{zcpkgs} is set up as a
collection, then @tt{raco setup} can target the third-party code
inside.

The drawback is that paths to modules in @tt{zcpkgs} would be tedious
to type. Assuming no use of a content addressing scheme, each path to
a module must contain many pieces of information that together form a
unique identifier. Recall that Java, for example, uses reversed domain
names to help create unique identifiers. Users will want
abbreviations, and the tooling must accomodate that.
