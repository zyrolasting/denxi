#lang scribble/manual

@require[@for-label[racket/base racket/file "zcpkg-settings.rkt"]
         "workspace.rkt"
         "zcpkg-settings.rkt"]

@title{A Zero-Collection Package Management System}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

@(define wsdir (tt CONVENTIONAL_WORKSPACE_NAME))
@(define depdir (tt CONVENTIONAL_DEPENDENCY_DIRECTORY_NAME))

@(define binary @(tt "zcpkg"))
@(define specfile @(tt CONVENTIONAL_PACKAGE_INFO_FILE_NAME))

@(define (tech/reference tag) (tech #:doc '(lib "scribblings/reference/reference.scrbl") tag))


This is a guide for @|binary|, a package manager for Racket.  In the context of
this manual, packages do not define collections in a Racket installation.

@section{Adding and Removing Packages}

As expected, you can create and install packages on your filesystem
for local development.

@verbatim|{
$ zcpkg my-package another
$ zcpkg install -y ./my-package ./another
}|

To fetch packages over a network, you request the packages using
special @deftech{package query strings} (henceforth "@deftech{queries}"). See
@secref{asking}.

@verbatim|{
$ zcpkg install -y john.doe:calculator
$ zcpkg uninstall -y john.doe:calculator
}|

Unless forced, @binary will not install any suspicious packages.  If
a situation comes up, then it will go over your options.

When @binary installs a package, it adds a @|depdir| directory to the package's
installation path. Package authors use this directory to access package modules
using the package provider's name and the package's name.

@racketblock[
(require (file "zcpkg-deps/john.doe/calculator/module.rkt"))
]

This illustrates a fundamental difference between @binary and @tt{raco pkg}:
@binary does not define @tech/reference{collections}. It only concerns itself
with fetching files and their dependencies safely and deterministically.
Packages interact strictly through the filesystem.

You can access the packages you installed for a specific use, outside of
conventions set by @|binary|. To do this, use the @litchar{link} command to
make a symbolic link to a package installed on your system.

@verbatim|{
$ zcpkg link john.doe:calculator calc
$ ls calc
}|

@section[#:tag "asking"]{Package Queries}

@tt{john.doe:calculator} means "the @tt{calculator} package by
@tt{john.doe}". What version of the package does this give you?
It turns out that @tt{john.doe:calculator} is just an abbreviation for
@tt{john.doe:calculator:draft:newest}.

@tt{draft} is the package's @tech{edition}. If you use
@tt{john.doe:calculator:scientific:newest}, then you're clarifying that you
want John's scientific calculator in particular. @tt{draft} is the default
edition for a given package.

@tt{newest} is the edition's @tech{revision}.  @tt{newest} is the default name
of the latest revision for a given edition.

To request an @italic{exact} version of a package, replace @tt{newest} with a
@tech{revision number} or another @tech{revision name}.

@verbatim|{
john.doe:calculator:scientific:288
john.doe:calculator:scientific:with-trig
}|

A package author can define revision names to help users avoid memorizing
revision numbers. The numbers are there for comparing versions, and for
providing a standard form for revisions. @tt{newest} is special: It is the only
@tech{revision name} that might not refer to the same implementation over time.

What about version ranges? When you ask for
@tt{john.doe:calculator:scientific:288}, you are actually asking for the latest
package in a range. That range just happens to contain only one package. You
can rewrite the query to make the interval explicit.

@verbatim|{
john.doe:calculator:scientific:288:288
}|

From here we can change the endpoints of the interval to accept alternative
packages.  This is useful in the event some package in the range is not
available.

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

There's a problem: A stranger can't read this and know why this query is
best. You can write a comment, but let's say John developed his scientific
calculator through an invite-only beta. John later put out a production-ready
copy in response to feedback, along with the beta revisions for posterity. You
can still use names in place of the numbers to express a preference for
revisions made during the closed beta.

@verbatim|{
john.doe:calculator:scientific:i:closed-beta:e:production
}|

When resolving @tech{revision names}, @binary will reject any query
that creates an invalid interval. Queries like those below will raise
an error.

@verbatim|{
john.doe:calculator:scientific:production:closed-beta
john.doe:calculator:scientific:9:0
}|

Formally, a @tech{package query string} follows this EBNF grammar:

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

@binary uses @tt{<name>}s for directory and link names on file systems. For
that reason, a package named @tt{aux} cannot appear on a Windows system.
@binary will alert you if a package uses a reserved file name.


@section{Workspace Directories}

All of @binary's disk activity takes place in a @deftech{workspace
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

@item{Everything @binary does is in this directory, so there isn't
any mystery about where caches or packages are.}

@item{Any one of your projects can have its own @wsdir, and therefore
its own configuration and dependencies. Each @wsdir is fully isolated
unless you go to your own lengths to link them together.}

@item{A workspace is a valid target for @tt{chroot}. @binary can
use Racket packages to construct an operating system or jailed
application.}

@item{You can define the actual root directory of your system
as a workspace for system-wide impact.}

]



@section[#:tag "new-pkg"]{Creating Packages}

Use the @tt{new} command to create a package.  You should see a new
directory with the name you provided.

@verbatim|{
$ zcpkg new my-pkg
$ ls my-pkg
}|

Inside that directory you should see @|specfile|. This is your @deftech{spec
file}. It defines your package, and should look something like this:

@racketmod[
info

(define provider "localhost.localdomain")
(define package "my-pkg")
(define edition "draft")
(define revision-number 0)
(define revision-names '())
(define dependencies '())
(define setup-module "setup.rkt")
]

If the @racket[provider] name is different, don't worry about that.  It is set
to the hostname of your machine.

If someone asks for @tt{localhost.localdomain:my-pkg:draft:0}, they
are going to get this package. That should clarify the role of the
first few lines.

If you set @racket[revision-names] to @racket['("initial" "oldest"
"beginning")], then users can replace the @racket[0] in their query with any of
those strings and still get this package. Choose revision names wisely: Once
you host the package, you will not be allowed to use the same revision names
again for the edition.

You can set @racket[dependencies] using the same strings discussed in
@secref{asking}.

@racketblock[(define dependencies '("john.doe:calculator" "alice:wonderland:small"))]

@racket[setup-module] is a path relative to the @tech{spec file}.
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


@section{Versioning}

A package version consists of an @tech{edition} and a @tech{revision}.

An @deftech{edition} is the name of a design. It acts as a semantic alternative
to a major version number. A @deftech{revision} is an implementation of an
edition.

An @deftech{revision number} is a non-negative integer. Every revision is
forever assigned the next available number in an edition.

A @deftech{revision name} is a custom alias for a @tech{revision number}. It
can be a user-defined string, but strings containing all digits, and the word
@racket{newest} are reserved.

A package must have a @tech{revision number}. When changed, a package must
increment its @tech{revision number} if the change uses the same
@tech{edition}. If the package starts a new edition, the @tech{revision number}
must reset to @racket[0].

The default name of an edition is @racket{draft}.

By the above rules, every package starts on the zeroth revision of the
@racket{draft} edition.

Use @litchar{zcpkg chver} to @bold{ch}ange the @bold{ver}sion of a package.

@itemlist[
@item{@litchar{zcpkg chver foo}: Increments @tt{foo}'s @tech{revision number}.}
@item{@litchar{zcpkg chver --edition nice --revision-number 0 foo fizz buzz}: Sets @tt{foo}'s edition to @racket{nice}, the @tech{revision number} to @racket[0], and the revision names to @racket['("fizz" "buzz")]}
]


@section{Configuration}

Let a hypothetical setting be “Use Widget”, which is a boolean. The value of
this setting is the last defined item in this list:

@itemlist[#:style 'ordered

@item{A hard-coded default.}

@item{A value bound to @tt{ZCPKG_USE_WIDGET} in @tt{:/etc/zcpkg.rktd}.}

@item{An environment variable named @tt{ZCPKG_USE_WIDGET}, which must be set to
a string containing a @racket[read]-able value
(e.g. @tt{ZCPKG_USE_WIDGET="#t"}).}

@item{A command-line flag named @litchar{--use-widget}. Since our example is a
boolean, the flag itself will suffice. Otherwise a string containing a
@racket[read]-able value must follow the flag.}

]

Using Racket literals in strings enables consistent behavior across
configuration sources, but it has a tradeoff: @litchar{--flag "foo"} means
@racket['foo] in the program, and @litchar{--flag '"foo"'} means
@racket["foo"]. @binary will account for these differences when it makes sense,
but be sure to think in terms of stringified Racket values when configuring it
from a command line or environment variables.

Use @litchar{zcpkg config} to manage @|binary|'s configuration:

@itemlist[
@item{@litchar{zcpkg config dump}: Prints a readable hash of the active configuration.}
@item{@litchar{zcpkg config set ZCPKG_VERBOSE "t"}: Changes a setting in @tt{/etc/zcpkg.rkt}.}
@item{@litchar{zcpkg config get ZCPKG_VERBOSE}: Gets the value of a setting}
]


@section{Hosting Packages}

You can serve packages using the @litchar{serve} command.

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


@section{Publishing Packages}

To prepare a package for use on a server, use the @litchar{bundle}
command.

@verbatim|{
$ zcpkg bundle my-pkg '\.rktd?$' '\.scrbl$'
}|

When you bundle a package, you specify the package's directory and
Perl-flavor regular expressions. The file paths matching the patterns
are included in the bundle.

The @litchar{bundle} command outputs an archive and an extended @tech{spec file}.

Use @litchar{-r} to use built-in patterns that match most Racket source modules.

@verbatim|{
$ zcpkg bundle -r my-pkg
}|

Use @litchar{-s} to write the output files where @litchar{zcpkg serve} can find
them.  This is to say that if you don't use @litchar{-s}, then you probably
intend to upload your bundle as a package author.

@verbatim|{
$ zcpkg bundle -rs my-pkg
}|

Finally, you can specify a private key to sign the bundle's digest.

@verbatim|{
$ zcpkg bundle --private-key-path key.pem -rs my-pkg
}|


@section{Capture and Restore}

@margin-note{Why not call it a lock file? To avoid confusion
with the same term in other contexts, e.g. @racket[make-lock-file-name].}
The @litchar{capture} command creates a @deftech{capture file}.  Check it into
source control or share it. It allows others to reproduce the workspace
on your system.

@verbatim|{
$ zcpkg capture > capture.rkt
}|

A capture file records the current @binary configuration, all installed
packages, and integrity information for files.  To capture a file, that file's
path must match at least one Perl regular expression provided in your command
line. If you do not specify a pattern, the @tt{capture} command will only
record integrity information for @tt{.rkt}, @tt{.rktd}, @tt{.ss}, @tt{.scrbl},
or @tt{.ss} files.

To explicitly capture @tt{.rkt} and @tt{.rktd} files, you could
write either of the following.

@verbatim|{
$ zcpkg capture '\.rktd?$' > capture.rkt
$ zcpkg capture '\.rkt$' '\.rktd$' > capture.rkt
}|

If you want to capture all integrity information for the sake of
reproducible builds, then specify an empty pattern. This trivially
matches any path.

@verbatim|{
$ zcpkg capture '' > capture.rkt
}|

Be careful in this case.  Racket bytecode files may differ beetween builds if,
say, a program embeds dates into generated code.

The @litchar{restore} command will attempt to reproduce the workspace recorded
in a @tech{capture file}. This may take a while if the capture is
large. @litchar{restore} will not modify the filesystem unless you grant
explicit consent in your command. If you do not consent, the @litchar{restore}
command will simply show you what it would do if you did consent.

@verbatim|{
$ zcpkg restore capture.rkt
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
zcpkg diff "capture.rkt"
}|

You can run the reported commands yourself to restore the workspace by hand,
or save them in an admin-friendly script.

The last command a restore operation performs is @litchar{zcpkg diff}, which
you can also run yourself. @litchar{diff} shows you any difference between a
@tech{workspace} and a capture file.

The output might look like this:

@verbatim|{
$ zcpkg diff capture.rkt
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

If the @litchar{diff} command has no output and a zero exit code,
then your workspace's files have the same content as recorded
in a given capture file.

Like other commands, @litchar{restore} targets @bold{an existing
workspace}. If you are restoring a capture, it will NOT delete files
inside that workspace. This means that the difference between the
workspace and the capture will show extra files.

If you want to limit noise when verifying results, use an empty
workspace. You can then move the created workspace to overwrite an
existing workspace, if you prefer.

@verbatim|{
$ mkdir zcpkg-workspace
$ zcpkg restore capture.rkt
$ zcpkg diff capture.rkt
$ mv zcpkg-workspace ../other/zcpkg-workspace
}|
