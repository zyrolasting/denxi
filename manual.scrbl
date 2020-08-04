#lang scribble/manual

@require[@for-label[racket/base racket/file "zcpkg-settings.rkt"]
         "workspace.rkt"
         "zcpkg-settings.rkt"]

@title{A Zero-Collection Package Management System}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

@(define wsdir (tt CONVENTIONAL_WORKSPACE_NAME))
@(define depdir (tt CONVENTIONAL_DEPENDENCY_DIRECTORY_NAME))

@(define binary @(tt "zcpkg"))
@(define definition @(tt CONVENTIONAL_PACKAGE_INFO_FILE_NAME))

@(define (tech/reference tag) (tech #:doc '(lib "scribblings/reference/reference.scrbl") tag))

This is a guide for @|binary|, a @tech/reference{collection}less package
manager for Racket.


@section{Concepts}

A @deftech{package} is a directory containing a @tech{package definition}.

Unlike packages defined for @tt{raco pkg}, @binary packages do not define
collections in a Racket installation. @binary concerns itself with the safe,
deterministic reproduction of source files and their dependencies in a
@deftech{workspace} directory, without any side-effect on the running Racket
installation. @secref{workspace} covers how a workspace decouples a running
system from any installed packages.

Also unlike @tt{raco pkg}, there is no concept of a package source.  In
general, all packages, regardless of where they come from, are identified by a
@deftech{query}. An @deftech{exact query} is expected to match @italic{the same}
implementation of a package @italic{every time}, whereas an @deftech{inexact
query} can match different (yet presumably equivalent) implementations.  An
exact query behaves like an ISBN does for books: If you send any two servers
the same @tech{exact query}, then you will get the same information.

It doesn't matter if the servers use JSON, XML, or HTML to express the packages
or their metadata. It doesn't matter if the server consults GitHub, a
filesystem, S3, or a sacrificial altar. A server's job is only to resolve
@tech{queries} to packages or related metadata. A server does not need to host
everything because of the ISBN-like relationship between @tech{exact queries}
and packages. @binary, as a client, merely needs to consult a second server in
the event the first cannot fulfill a request for information.

There are human elements that make determinism hard to guarentee.  The
@secref{verification} section covers how to use @binary to reproduce an exact
@tech{workspace}.


@section{Adding and Removing Packages}

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

This illustrates the fundamental difference between @binary and @tt{raco pkg}:
@binary does not define @tech/reference{collections}. Packages interact
strictly through the filesystem.

You can access the packages you installed for a specific use, outside of
conventions set by @|binary|. To do this, use the @litchar{link} command to
make a symbolic link to a package installed on your system.

@verbatim|{
$ zcpkg link john.doe:calculator calc
$ ls calc
}|

@section[#:tag "asking"]{Package Queries}

You can request packages using a colon-separated @tech{query} string.

@tt{john.doe:calculator} means "the @tt{calculator} package by
@tt{john.doe}". But it turns out that @tt{john.doe:calculator} is just an
abbreviation for @tt{john.doe:calculator:draft:newest}.  @tt{draft} is the
package's @tech{edition}. @tt{newest} is the edition's @tech{revision}.  Both
@tt{draft} and @tt{newest} are just default values.

So, @tt{john.doe:calculator:draft:newest} means "the latest revision of the
draft edition of the @tt{calculator} package by @tt{john.doe}."

If you prefer a scientific calculator, the package author can provide that
design as a diferent edition. In that case you replace @tt{draft} with
@tt{scientific}.

@verbatim|{
john.doe:calculator:scientific:newest
}|

To request an @italic{exact} version of a package, replace @tt{newest} with a
@tech{revision number} or another @tech{revision name}. The following examples
are @tech{exact queries} because they request specific implementations of
John's scientific calculator.

@verbatim|{
john.doe:calculator:scientific:288
john.doe:calculator:scientific:with-trig
}|

Revision names are aliases or revision numbers, but the numbers are necessary
to comparing versions and provide a standard form for revisions. @tt{newest} is
special for being the only @tech{revision name} that can refer to more than one
implementation. In other words, all queries that use @tt{newest} are
@tech{inexact queries}.

What about version ranges? When you ask for
@tt{john.doe:calculator:scientific:288}, you are actually asking for the latest
package in an inclusive interval that just happens to contain only one
package. You can rewrite the query to make this interval explicit.

@verbatim|{
john.doe:calculator:scientific:288:288
}|

From here we can change the endpoints of the interval to accept alternative
packages.  This is useful if some implementations are not available.

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

There's a problem: Someone can't read this and know why this query makes sense
for your project. You can write a comment, but let's say John developed his
scientific calculator through an invite-only beta. John later put out a
production-ready copy in response to feedback, along with the beta revisions
for posterity. You can still use names in place of the numbers to express a
preference for revisions made during the closed beta.

@verbatim|{
john.doe:calculator:scientific:i:closed-beta:e:production
}|

When resolving @tech{revision names}, @binary will reject queries
like these because they each create an invalid interval:

@verbatim|{
john.doe:calculator:scientific:production:closed-beta
john.doe:calculator:scientific:9:0
john.doe:calculator:scientific:e:3:e:3
}|

Formally, a @tech{query} string follows this EBNF grammar:

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


@section[#:tag "workspace"]{Workspace Directories}

All of @binary's disk activity takes place in an initally empty
@tech{workspace} directory called @|wsdir|. @binary organizes files in
@wsdir according to the
@hyperlink["https://refspecs.linuxfoundation.org/FHS_3.0/fhs/index.html"]{Filesystem
Heirarchy Standard v3.0}. This makes a @tech{workspace} a valid target for
@tt{chroot}. You can therefore work with @binary to construct a bootable or
jailed application.

Everything related to @binary is in a @tech{workspace}, including configuration, logs,
cached files, and installed packages. Any one of your projects can have its own @wsdir,
and therefore its own configuration and dependencies. Each @wsdir is isolated
unless you link them together yourself. You can define the actual root
directory of a Linux system as a workspace for system-wide impact.

On initialization, @binary will search for an existing @|wsdir| directory.  It
first checks if it is a subdirectory of @racket[(current-directory)].  Failing
that, it will check each parent directory for @|wsdir|. If @wsdir does not exist, then
@binary will create a new one in @racket[(current-directory)].


@section[#:tag "new-pkg"]{Creating Packages}

Use the @tt{new} command to create a package.  You should see a new
directory with the name you provided.

@verbatim|{
$ zcpkg new my-pkg
$ ls my-pkg
}|

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
(define racket-versions '(("6.0" . "7.7.0.5")))
]

If the @racket[provider] name is different, then that's okay.  It is set to the
hostname of your machine.

For this example, the @tech{query} @tt{localhost.localdomain:my-pkg:draft:0}
refers to this package. You can see that a query consists of the
@racket[provider], @racket[package], @racket[edition], and
@racket[revision-number] fields.

If you set @racket[revision-names] to @racket['("initial" "oldest"
"beginning")], then users can replace the @racket[0] in their query with any of
those strings and still get this package. Choose revision names wisely: Once
you host the package, you will not be allowed to use the same revision names
again for the edition.

You can set @racket[dependencies] using the same strings discussed in
@secref{asking}.

@racketblock[(define dependencies '("john.doe:calculator" "alice:wonderland:small"))]

@racket[racket-versions] is a list of pairs, where each pair is an inclusive
interval of Racket versions you support for this package. Gaps in versions
are not expected, but you can express them for flexibility.

@racket[tags] is a list of strings you can set for discovery purposes when
users search for packages.

@racketblock[(define tags '("video game" "3d" "action"))]

@racket[description] is a short summary of your package. It will also be
checked in user searches.

@racketblock[(define description "A shoot-em up game built with 'universe'")]

@racket[home-page] is an URL to a related web page for your package.

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
$ zcpkg sandbox mavrick:pyracket:totally-works
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
$ zcpkg sandbox --sandbox-path-permissions "[...]" \
                --sandbox-network-permissions "[***]" \
                -e "(install-python3)" \
                mavrick:pyracket:totally-works
}|


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

Let a hypothetical setting be “Use Widget”, which is a boolean.  The canonical
identifer for of the setting is @racket[ZCPKG_USE_WIDGET], meaning we
capitalize the letters, replace spaces with underscores, and prefix the result
with @racket[ZCPKG_].

The value of @racket[ZCPKG_USE_WIDGET] is the last defined item in this list:

@itemlist[#:style 'ordered

@item{A hard-coded default.}

@item{A value bound to @tt{ZCPKG_USE_WIDGET} in a @tech{workspace}'s @tt{etc/zcpkg.rktd}.}

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

Use the @litchar{zcpkg config} command to manage @|binary|'s configuration. I'll
use the @tt{ZCPKG_VERBOSE} setting for the below examples:

@itemlist[
@item{@litchar{zcpkg config dump}: Prints a readable hash of the active configuration.}
@item{@litchar{zcpkg config set ZCPKG_VERBOSE "t"}: Changes a setting in a @tech{workspace}'s @tt{etc/zcpkg.rkt} file.}
@item{@litchar{zcpkg config get ZCPKG_VERBOSE}: Gets the value of a setting}
]


@section{Hosting Packages}

You can serve packages using the @litchar{serve} command.

@verbatim|{
$ zcpkg serve
}|

The server keeps its own directory of packages bundled into archives.

@tt{GET /info/<query>} returns the contents of a @tech{package definition} in a
UTF-8 @racket[#"text/plain"] body.  The selected package definition corresponds
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

The @litchar{bundle} command outputs an archive and an extended @tech{package definition}.

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


@section[#:tag "verification"]{Verification}

To keep things relatively sane, we need reproducible and deterministic builds.
This means that for the same input, we get the exact same files.

Unfortunately, this is a high bar to reach unless you keep a copy of all
dependencies with your project. Racket bytecode files may differ even if they
behave the same when executed, simply because they embed a changing date in
their code. A @tech{workspace} might also contain different configurations. On
top of that, @tech{revision names}, being provider-asserted, can refer to
different revision numbers on different servers. @binary includes tools to
capture files in @tech{workspace}, such that only the files you declare
are not expected to change.

@margin-note{Why not call it a lock file? To avoid confusion with the same term
in other contexts, e.g. @racket[make-lock-file-name].}  The @litchar{capture}
command creates a @deftech{capture file}.  Check this file into source control
or share it. It allows others to reproduce a workspace on your system.

@verbatim|{
$ zcpkg capture > capture.rkt
}|

A capture file records the current @binary configuration, all installed
packages, and integrity information for files. It does NOT follow or capture
symbolic links. To capture a file, that file's path must match at least one
Perl regular expression provided in your command line. If you do not specify a
pattern, the @tt{capture} command will only record integrity information for
@tt{.rkt}, @tt{.rktd}, @tt{.ss}, @tt{.scrbl}, or @tt{.ss} files.

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
