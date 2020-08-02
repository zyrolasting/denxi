#lang scribble/manual

@require[@for-label[racket/base "zcpkg-settings.rkt"]
         racket/runtime-path
         "workspace.rkt"
         "zcpkg-settings.rkt"]

@title{A Zero-Collection Package Management System}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

@(define-runtime-path white-paper.scrbl "white-paper.scrbl")
@(define wsdir (tt CONVENTIONAL_WORKSPACE_NAME))
@(define depdir (tt CONVENTIONAL_DEPENDENCY_DIRECTORY_NAME))

@(define binary @(tt "zcpkg"))
@(define specfile @(tt "zcpkg.rktd"))

This guide covers the @binary package manager. For why we need
another package manager, see @other-doc[white-paper.scrbl].


@section{Adding and Removing Packages}

As expected, you can create and install packages on your filesystem
for local development.

@verbatim|{
$ zcpkg new my-package another
$ zcpkg install -y ./my-package ./another
}|

To fetch packages over a network, you request the packages using
special @deftech{package query strings} (henceforth "@deftech{queries}"). See
@secref{asking}.

@verbatim|{
$ zcpkg install -y sagegerard.com:polyglot:draft
$ zcpkg uninstall -y john.doe:calculator
}|

Unless forced, @binary will not install any suspicious packages.  If
the situation comes up, then it will go over your options.


@section[#:tag "asking"]{How to Ask for a Package}

When you install @tt{john.doe:calculator}, what version of the package
do you get? Where is it coming from?


@subsection{Understanding Short Names}

You are meant to read @tt{john.doe:calculator} as "the
@tt{calculator} package by @tt{john.doe}". This short query
implies that you want the latest draft of the package.
Let's add our own version information for comparison.

First, you can specify an @tech{edition}. Just add another colon and
the name of the edition.  If you use
@tt{john.doe:calculator:scientific}, then you're clarifying that you
want John's scientific calculator in particular.  It turns out that
@tt{john.doe:calculator} is actually understood as
@tt{john.doe:calculator:draft} because the @tt{draft} edition is the
default.

But hang on, @tt{john.doe:calculator:draft} is still ambiguous.
We need to specify a @tech{revision}, so that you get an exact
implementation. It turns out that if you omit a revision,
then @binary will look for the newest one. That revision
is @italic{always called} @tt{newest}.

Therefore, @tt{john.doe:calculator} is an abbreviation for
@tt{john.doe:calculator:draft:newest}.


@subsection{Selecting an Exact Version}

When you want to depend on a particular version of a package, you can
just replace @tt{newest} with a @tech{revision number} or another
@tech{revision name}.

@verbatim|{
john.doe:calculator:scientific:288
john.doe:calculator:scientific:with-trig
}|

You are not expected to memorize revision numbers, which is why the
author can define revision names to help you select an implementation.
The numbers are only there for comparing versions, or for helping you
get a version even if there is no name for it.

@tt{newest} is the only name that might not refer to the same package
over time. For that reason, it is the only @tech{revision name} that
prevents a query from specifying an exact version.


@subsection{Selecting a Version Range}

When you ask for @tt{john.doe:calculator:scientific:288}, you are
actually asking for a range of packages that contains one member.
In integer interval notation, you're asking for @tt|{{288 .. 288}}|.

You can rewrite the query to make the interval explicit.

@verbatim|{
john.doe:calculator:scientific:288:288
}|

While redundant, this form lets us expand our search to more packages.

@verbatim|{
john.doe:calculator:scientific:102:288
}|

You can add flags to mark the interval as inclusive or exclusive of
each endpoint. Use the letter @tt{i} for inclusive, and @tt{e} for
exclusive.  In the below form, revision @tt{288} will @italic{not} be
included because of the @tt{e} right next to it.

@verbatim|{
john.doe:calculator:scientific:i:102:e:288
}|

There's a problem: A stranger can't read this and know why this query
is best. You can write a comment, or the package author can name their
revisions.

Let's say John developed his scientific calculator through an
invite-only beta. John later put out a production-ready copy in
response to feedback, along with the beta revisions for posterity. You
can still use names in place of the numbers to express a preference
for revisions made during the closed beta.

@verbatim|{
john.doe:calculator:scientific:i:closed-beta:e:production
}|

When resolving @tech{revision names}, @tt{zcpkg} will reject any query
that creates an invalid interval. Queries like those below will simply
not work.

@verbatim|{
john.doe:calculator:scientific:production:closed-beta
john.doe:calculator:scientific:9:0
}|


@subsection{Grammar}

A @tech{package query string} is a namespace-specific string (or NSS)
in an experimental URN namespace.  That's a ten-dollar way of saying
that if you took one of the strings we were talking about and shoved
@tt{urn:example:} right in front of it, you'd get a valid URN. You
only type the NSS to avoid repeating the first part.

A given NSS follows this EBNF grammar:

@verbatim|{
<package-query> ::= <package-identity> | <package-identity> ":" <version>

<package-identity> ::= <name> ":" <name>

<version> ::= <name> | <name> ":" <revision>

<revision> ::= <revision-boundary> | <inclusive-revision-range> | <general-revision-range>

<revision-boundary> ::= <name> | <revision-number>

<revision-number> ::= <non-zero-digit> <digit>*

<inclusive-revision-range> ::= <revision-boundary> ":" <revision-boundary>

<general-revision-range> ::= <interval-flag> ":" <revision-boundary> ":" <interval-flag> ":" <revision-boundary>

<interval-flag> ::= "e" | "i"

<non-zero-digit> ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

<name> ::= ? A string representing a legal file name ?
}|


The @tt{<name>} terminal should scare you. @tt{<name>}s are used for
directory and link names on file systems. There are two reasons for
this:

@itemlist[#:style 'ordered

@item{Defining a restricted set of names is a slippery slope to
preferring the alphabet of the maintainers.}

@item{If something goes wrong with files written by @tt{zcpkg}, then
you don't need @tt{zcpkg} to fix the issue. If I used a content
addressing scheme that puked base64-encoded hashes all over your drive,
then that wouldn't be the case.}

]

Still, this is a judgement call that allows some bugs. If you publish
a package named @tt{aux}, then you will get a support ticket from
Windows users. I'm open to reconsidering the @tt{<name>} definition if
this causes too much pain. For now, the benefits are helpful.


@section{The Workspace}

All of @tt{zcpkg}'s disk activity takes place in a @deftech{workspace
directory} called @|wsdir|. @wsdir starts empty, but added files are
organized according to the
@hyperlink["https://refspecs.linuxfoundation.org/FHS_3.0/fhs/index.html"]{Filesystem
Heirarchy Standard v3.0}.

When there is no room for confusion, I will call this directory the
@deftech{workspace}. @binary will first check to see if @wsdir is a
subdirectory of @racket[(current-directory)].  Failing that, it will
check every parent directory in a walk towards a root directory. If
@wsdir does not exist, then @binary will assume that it needs to
create a new one in @racket[(current-directory)].

When I refer to a path using a leading @tt{:}, as in
@tt{:/usr/local}, I am referring to a path relative to the
@wsdir used by the package manager.

Doing it this way has some benefits:

@itemlist[

@item{Everything @tt{zcpkg} does is in this directory, so there isn't
any mystery about where caches or packages are.}

@item{Any one of your projects can have its own @wsdir, and therefore
its own configuration and dependencies. Each @wsdir is fully isolated
unless you go to your own lengths to link them together.}

@item{A workspace is a valid target for @tt{chroot}. @tt{zcpkg} can
use Racket packages to construct an operating system or jailed
application.}

@item{You can define the actual root directory of your system
as a workspace for system-wide impact.}

]



@section[#:tag "new-pkg"]{Creating a Package}

Use the @tt{new} command to create a package.  You should see a new
directory with the name you provided.

@verbatim|{
$ zcpkg new my-pkg
$ ls my-pkg
}|

Inside that directory you will find a few files.
Open up @|specfile|. It should look like this:

@racketblock[
#:provider "localhost.localdomain"
#:package "my-pkg"
#:edition "draft"
#:revision-number 0
#:revision-names ()
#:dependencies ()
#:setup-module "setup.rkt"
]

This is your @deftech{spec file}. It defines metadata for your
package.

If someone asks for @tt{localhost.localdomain:my-pkg:draft:0}, they
are going to get this package. That should clarify the role of the
first few lines.

If you set @racket[#:revision-names] to @racket[("initial" "oldest"
"beginning")], then users can replace that @racket[0] with any of
those strings to reference this package. Choose those names wisely:
Once you host the package, you will not be allowed to use the same
revision names again for the edition!

You can set your @racket[#:dependencies] using the same strings
discussed in @secref{asking}.

@racketblock[#:dependencies ("john.doe:calculator" "alice:wonderland:small")]

@racket[#:setup-module] is a path relative to the @tech{spec file}.
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
$ zcpkg sandbox mavrick:pyracket:totally-works
>
}|

By default, the sandbox prohibits all file and network I/O, and limits
both the time and memory the setup module can consume. Since the
sandbox uses your kernel, it is not a substitute for OS-level
security.  See @racketmodname[racket/sandbox] for more information.

The @tt{new} command also put a @tt{show-help} procedure in the setup
module.  @tt{show-help} is a conventional procedure. It gives a
package author a chance to describe what the package can do, and what
permissions are necessary to opt-in to features that a Racket package
would normally not do.  With consent, you can automate setup of
non-Racket resources before using other features in the package.

@verbatim|{
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
package, remember that your @tech{workspace} is a valid target for
@tt{chroot}. This protects your filesystem outside of the workspace,
and helps package authors perform more helpful tasks.



@section{Configuration}

Both the package manager and service use a configuration space derived
from English labels of individual settings.

Let a hypothetical setting be “Use Widget”, which is a boolean.

In this case, the value of this setting is the last item in this list
to define a value:

@itemlist[#:style 'ordered

@item{A hard-coded default.}

@item{A @racket[read]-able literal by the @racket[#:ZCPKG_USE_WIDGET] keyword in @tt{:/etc/zcpkg.rktd}.}

@item{An environment variable named @tt{ZCPKG_USE_WIDGET}.}

@item{A command-line flag named @litchar{--use-widget}. If the value is a
boolean, the flag itself will suffice. Otherwise it is a string
containing a @racket[read]-able value as a literal. Shortened
command-line flags are defined independently.}

]


@section{Hosting Packages}

You can serve packages using the @tt{serve} command.

@verbatim|{
$ zcpkg serve
}|

The server keeps its own directory of packages bundled into archives.

@tt{GET /info/<query>} returns the contents of a @tech{spec file} in a
UTF-8 @racket[#"text/plain"] body.  The selected spec file corresponds
to the @italic{latest} revision of a package named in @tt{<query>}.
In addition to the fields defined in @secref{new-pkg}, the metadata
will include integrity information and a signature for the package.

@tt{GET /artifact/<query>} behaves like @tt{/info}, except the response
is a GZipped TAR archive file containing the package.

Currently, the service does not support uploading packages.  Adding
packages to the service is a manual process for an administrator.

@subsection{Bundling a Package}

To prepare a package for use on a server, use the @tt{bundle}
command.

@verbatim|{
$ zcpkg bundle my-pkg '\.rktd?$' '\.scrbl$'
}|

When you bundle a package, you specify the package's directory and
Perl-flavor regular expressions. The file paths matching the patterns
are included in the bundle.

The @tt{bundle} command outputs an archive and an extended @tech{spec
file} with integrity information and a signature, if you specify a
private key.

Use @tt{-r} to use built-in patterns that match most Racket source
modules.

@verbatim|{
$ zcpkg bundle -r my-pkg
}|

Use @tt{-s} to write the output files where @tt{zcpkg serve} can find them.

@verbatim|{
$ zcpkg bundle -rs my-pkg
}|


@section{Capture and Restore}

Once you are satisfied with the packages installed on your system, you
can capture a workspace for a teammate or end-user to restore.

@subsection{Capturing a Workspace}
@margin-note{Why not call it a lock file? To avoid confusion
with the same term in other contexts, e.g. @racket[make-lock-file-name].}

The @tt{capture} command creates a @deftech{capture file}.

@verbatim|{
$ zcpkg capture > capture.rktd
}|

A capture file records the current @tt{zcpkg} configuration, all
installed packages, and the integrity information for files.  The
files captured must match Perl regular expressions you pass in your
command line. If you do not specify a pattern, the @tt{capture}
command will only record integrity information for @tt{.rkt},
@tt{.rktd}, @tt{.ss}, @tt{.scrbl}, or @tt{.ss} files.

To explicitly capture @tt{.rkt} and @tt{.rktd} files, you could
write either of the following.

@verbatim|{
$ zcpkg capture '\.rktd?$' > capture.rktd
$ zcpkg capture '\.rkt$' '\.rktd$' > capture.rktd
}|

If you want to capture all integrity information for the sake of
reproducible builds, then specify an empty pattern. This trivially
matches any path.

@verbatim|{
$ zcpkg capture '' > capture.rktd
}|

Be careful about when you capture the workspace in this case.  If you
include Racket bytecode, then that bytecode may differ between
workspaces in the event, say, the Racket expander embeds dates into a
program.


@section{Restore from a Capture}
The restore command will attempt to reproduce the workspace recorded
in a @tech{capture file}. This may take a while if the capture is large.

@tt{restore} will not modify the filesystem unless you grant
explicit consent in your command. If you do not consent, the
@tt{restore} command will simply show you what it would do if you did
consent.

@verbatim|{
$ zcpkg restore capture.rktd
# These commands install packages under a specific configuration.
# The configuration controls whether zcpkg trusts unsigned
# packages or bad digests, so please review this carefully.
#
# You can adapt this output to a script on your operating system, or you can
# run the restore command again with -y to execute these instructions.

zcpkg config set ZCPKG_MATCH_RACKET_MODULES "#f"
zcpkg config set ZCPKG_INSTALL_ORPHAN "#f"
zcpkg config set ZCPKG_TRUST_BAD_SIGNATURE "#f"
zcpkg config set ZCPKG_BUNDLE_FOR_SERVER "#f"
zcpkg config set ZCPKG_COLORIZE_OUTPUT "#f"
zcpkg config set ZCPKG_SANDBOX_EVAL_TIME_LIMIT_SECONDS "300"
zcpkg config set ZCPKG_DOWNLOAD_IGNORE_CACHE "#f"
zcpkg config set ZCPKG_VERBOSE "#f"
zcpkg config set ZCPKG_REVISION_ZERO "#f"
zcpkg config set ZCPKG_TRUST_UNSIGNED "#f"
zcpkg config set ZCPKG_SANDBOX_PATH_PERMISSIONS "()"
zcpkg config set ZCPKG_SANDBOX_EVAL_MEMORY_LIMIT_MB "10"
zcpkg config set ZCPKG_SANDBOX_MEMORY_LIMIT_MB "30"
zcpkg config set ZCPKG_EDITION "#f"
zcpkg config set ZCPKG_INSTALL_RELATIVE_PATH "\"usr/lib/racket\""
zcpkg config set ZCPKG_PRIVATE_KEY_PATH "#f"
zcpkg config set ZCPKG_CONSENT "#f"
zcpkg config set ZCPKG_TRUST_BAD_DIGEST "#f"
zcpkg config set ZCPKG_SANDBOX_NETWORK_PERMISSIONS "(#f #f #f #f)"
zcpkg config set ZCPKG_SERVICE_ENDPOINTS "((\"default\" . \"http://localhost:8080\"))"
zcpkg config set ZCPKG_LEAVE_ORPHANS "#t"
zcpkg config set ZCPKG_PATH_LENGTH_BUDGET "4096"
zcpkg config set ZCPKG_DOWNLOAD_MAX_REDIRECTS "2"
zcpkg config set ZCPKG_TRUST_UNVERIFIED_HOST "#f"
zcpkg install -y "localhost.localdomain:bar:draft:i:4:i:4"
zcpkg install -y "localhost.localdomain:baz:draft:i:3:i:3"
zcpkg install -y "localhost.localdomain:baz:draft:i:3:i:3"
zcpkg install -y "localhost.localdomain:foo:draft:i:0:i:0"
zcpkg diff "capture.rktd"
}|

You can run the reported commands yourself to restore the workspace by hand,
or save them in an admin-friendly script.

@subsection{Verifying a Restore Operation}

The last command a restore operation performs is @tt{zcpkg diff}, which
you can also run yourself. @tt{diff} shows you any difference between
a @tech{workspace} and a capture file.

The output might look like this:

@verbatim|{
$ zcpkg diff capture.rktd
- usr/lib/racket/localhost.localdomain/bar/draft/4/setup.rkt
* usr/lib/racket/localhost.localdomain/baz/draft/3/setup.rkt
+ usr/lib/racket/localhost.localdomain/foo/draft/0/zcpkg-deps/localhost.localdomain/bar/great.rkt
* usr/lib/racket/localhost.localdomain/foo/draft/0/zcpkg-deps/localhost.localdomain/baz/setup.rkt
* usr/lib/racket/localhost.localdomain/baz/draft/cool/setup.rkt
- usr/lib/racket/localhost.localdomain/foo/draft/0/mod.rkt
- usr/lib/racket/localhost.localdomain/foo/draft/0/zcpkg-deps/localhost.localdomain/bar/setup.rkt
+ usr/lib/racket/localhost.localdomain/bar/draft/4/great.rkt
+ usr/lib/racket/localhost.localdomain/foo/draft/0/fo.rkt
}|

@itemlist[
@item{@tt{+} means this file exists in your workspace, and not in the captured workspace.}
@item{@tt{-} means this file exists in the captured workspace, but not in yours.}
@item{@tt{*} means this file exists in both workspaces, but has different content.}
]

The paths contain redundant information. For example, these two paths
reference the same file.

@verbatim|{
+ usr/lib/racket/localhost.localdomain/foo/draft/0/zcpkg-deps/localhost.localdomain/bar/great.rkt
+ usr/lib/racket/localhost.localdomain/bar/draft/4/great.rkt
}|

This is because the former path uses a symbolic link to reference a
dependency.  They are both included in the output as a visual clue to
what installed packages are affected by discrepencies.

If the @tt{diff} command has no output and a zero exit code,
then your workspace's files have the same content as recorded
in a given capture file.

@subsection{Restoring on a Clean Slate}

Like other commands, @tt{restore} targets @bold{an existing
workspace}. If you are restoring a capture, it will NOT delete files
inside that workspace. This means that the difference between the
workspace and the capture will show extra files.

If you want to limit noise when verifying results, use an empty
workspace. You can then move the created workspace to overwrite an
existing workspace, if you prefer.

@verbatim|{
$ mkdir zcpkg-workspace
$ zcpkg restore capture.rktd
$ zcpkg diff capture.rktd
$ mv zcpkg-workspace ../other/zcpkg-workspace
}|


@section{Using Installed Modules in Code}

As the name implies, zero-collection packages add no collection paths
to a Racket installation. The modules are only generally accessible
using @racket[file].

Every installed package is given a directory named @|depdir|.
Using it, you can access package modules via a provider
name and a package name. The version information is not
included in the path.

@racketblock[
(require (file "zcpkg-deps/john.doe/calculator/module.rkt"))
]

This is why a package cannot function without being installed in a
@tech{workspace}.


@section{Linking Packages}

The @tt{link} command finds a package on the local system and
makes a symbolic link pointing to it.

@verbatim|{
$ zcpkg link john.doe:calculator:scientific calc
$ ls calc
}|

This is useful for creating a file tree that does not need to follow
the conventions set by @tt{zcpkg}. Linking packages is also a way to
use multiple versions of the same package in a program.


@section{Versioning}

Instead of major versions and other numbers of decreasing major-ness,
ZCP versions use @tech{editions} and @tech{revisions}.

@subsection{Rules}

An @deftech{edition} is a named sequence of revisions. A
@deftech{revision} is a state of an edition at a moment in time.

An @deftech{revision number} is a non-negative integer. Every revision
is forever assigned the next available number in an edition.

A @deftech{revision name} is a custom alias for a @tech{revision
number}. It can be a user-defined string, but strings containing all
digits, and the word @racket{newest} are reserved.

A package must have a @tech{revision number}. When changed, a
package must increment its @tech{revision number} if the change
uses the same @tech{edition}. If the package starts a new edition, the
@tech{revision number} must reset to @racket[0].

The default name of an edition is @racket{draft}.

By the above rules, every package starts on version @racket{draft:0}.


@subsection{Updating Package Versions}

Use @tt{zcpkg chver} to @bold{ch}ange the @bold{ver}sion of a package.

Given a package @tt{foo}:

@itemlist[
@item{@tt{zcpkg chver foo}: Increments @tt{foo}'s @tech{revision number}.}
@item{@tt{zcpkg chver --new-edition nice foo}: Sets @tt{foo}'s edition to @tt{nice}, and the @tech{revision number} to @racket[0].}
]
