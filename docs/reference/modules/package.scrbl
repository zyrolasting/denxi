#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/string
                    xiden/input
                    xiden/message
                    xiden/package
                    xiden/racket-module
                    xiden/string
                    xiden/subprogram
                    xiden/url
                    xiden/version]
         @for-syntax[xiden/package]
         xiden/package
         "../../shared.rkt"]

@title{Packages}

@defmodule[xiden/package]

@defstruct*[package ([description string?]
                     [tags (listof non-empty-string?)]
                     [url url-string?]
                     [provider non-empty-string?]
                     [name non-empty-string?]
                     [edition non-empty-string?]
                     [revision-number revision-number?]
                     [revision-names (listof non-empty-string?)]
                     [os-support (listof symbol?)]
                     [racket-versions (listof (list/c non-empty-string?))]
                     [metadata (hash/c symbol? string?)]
                     [inputs (listof package-input?)]
                     [output-names (listof non-empty-string?)]
                     [build (-> non-empty-string? (subprogram/c void?))])]{
A @deftech{package} is an instance of @racket[package].

@racket[description] is a human-readable summary of the package's purpose.

@racket[tags] is a list of human-readable topics used for discovery.

@racketid[url] is the primary, or canonical URL used to guide a user
towards more information (as opposed to secondary URLs that may appear
in @racket[metadata]).

@racket[provider] is the name of the allegedly responsible
distributor.

@racket[name] is the name of the package.

@racket[edition], @racket[revision-number], and
@racket[revision-names] are the package's @tech{edition},
@tech{revision number}, and @tech{revision names}.

@racket[os-support] is a list of possible values from
@racket[(system-type 'os)].  If @racket[(system-type 'os)] is not an
element of @racket[os-support], then either calls to @racket[build]
will fail, or the software created with @racket[build] will not
function.

@racket[racket-versions] is a list of Racket version ranges that
should be interpreted as a set of supported Racket versions. If
@racket[(version)] is not an element of any version interval, then
assume that the software created with @racket[build] will not
function with the running version of Racket.

@racket[metadata] is a hash table of user-defined metadata.  In the
event entries of this table appear redundant with other structure
fields, prefer the values in the structure fields.

@racket[inputs] is a list of @tech{package inputs}.

@racket[output-names] is a list of defined @tech{package outputs}.

@margin-note{@racket[build] procedures created using
@racketmodname[xiden/pkgdef] are always surjective, but might not be
injective.}
@racket[build] is function that maps the elements of
@racket[output-names] to @tech{subprograms}. Each subprogram
procedure installs software into @racket[current-directory] assuming
@racket[current-inputs] is bound to @racket[inputs]. The behavior of
@racket[build] is impacted by the @tech{runtime configuration}.

Xiden will not verify if @racket[build] procedures are bijective.  If
@racket[build] is not bijective, then @racket[build]'s relationship
with the host system varies slightly. If @racket[build] is not
injective, then it may create redundant data on disk because Xiden
assumes that different output names imply different file
distributions. If @racket[build] is not surjective, then a
@tech{subprogram} might be inaccessible.  This can happen if a
@racket[package] instance is manually created with faulty data.
Bijective @racket[build] procedures do not have these problems.
}

@defthing[empty-package package?]{
The @tech{package} with no inputs, no outputs, and all default values.
The empty package claims to support all operating systems and versions
of Racket.
}

@defthing[output-not-found (-> non-empty-string? subprogram?)]{
The build procedure for the empty package.

Returns a @tech{subprogram} that always fails and adds
@racket[$package:output:undefined] to the program log.
}

@defproc[(install [link-path (or/c #f path-string?)]
                  [output-name (or/c #f string?)]
                  [package-definition-variant any/c])
                  subprogram?]{
Returns a @tech{subprogram} called for its effect.  The effect
being that a symbolic link gets created at @racket[link-path],
pointing to a directory. That directory contains the files
corresponding to the @racket[output-name] defined in
@racket[package-definition-variant]).

If @racket[link-path] is @racket[#f], then the name of the symbolic
link will match the name of the package.

If @racket[output-name] is @racket[#f], then @racket[install] will use
@racket[DEFAULT_STRING].

The @tech{subprogram} is not atomic, so failure may result in
a broken intermediate state on disk. This procedure should be used
in the context of a transaction to avoid this problem.

All @racket[install] @tech{messages} are instances of @racket[$package].
}


@defthing[current-package-definition-editor
          (parameter/c (-> bare-racket-module?
                           (or/c bare-racket-module?
                                 (subprogram/c bare-racket-module?))))]{
A parameter for a procedure that replaces any @tech{bare}
@tech{package definition} with another before using that definition.

This procedure defaults to the identity function, so no code is
replaced. The procedure may instead return a @tech{subprogram} if it
intends to add to a @tech{subprogram log}.

This procedure is useful for standardizing definitions, or for
analyzing builds. @bold{Define with care.} This procedure may override
all package definitions, which can render a Xiden process inoperable
or unsafe.

Take for example a function that returns the same static package
definition, which has one dependency.

@racketblock[
(current-package-definition-editor (lambda (original)
  (struct-copy bare-racket-module original
               [code
                 '((input "pkgdef" (sources "https://example.com/other.rkt"))
                   (output "default"
                           pkgdef-input := (input-ref "pkgdef")
                           pkgdef-path := (resolve-input "pkgdef")
                           (install #f #f pkgdef-path)))])))
]

This creates builds that will not terminate. Even if Xiden downloads a
new package definition from @racket{https://example.com/other.rkt}, it
will only be replaced by another instance of the same data returned
from @racket[(current-package-definition-editor)].
}

@defthing[current-package-editor
          (parameter/c (-> package? (or/c package? (subprogram/c package?))))]{
Like @racket[current-package-definition-editor], but for parsed
packages that are suitable for installation.

This procedure defaults to the identity function, so the package is
used as-is.

This parameter would likely be easier to use in a @tech{launcher},
since struct bindings are available to operate on the actual package.
}


@defproc[(sxs [pkg package?]) (subprogram/c package?)]{
Returns a @tech{subprogram} that functionally updates
@racket[(package-provider pkg)] with a cryptographically random name,
and adds a @racket[$show-string] @tech{message} to the
@tech{subprogram log} that displays as @litchar{sxs: <old name> ~>
<new name>}.

When used as the @racket[current-package-editor], Xiden is forced into
an extreme interpretation of side-by-side (SxS) installations.

In this mode, package conflicts become vanishingly improbable. The
cost is that Xiden's cycle detection and caching mechanisms are
defeated because they will never encounter the same package enough
times for them to matter.  Installations using @racket[sxs] consume
more resources, and are vulnerable to non-termination. Stop using
@racket[sxs] the moment you don't need it.

@racket[sxs] is useful for handling unwanted
@tech/xiden-tutorials{package conflicts}. Re-running a single
installation using @racket[sxs] will force installation of conflicting
packages without disturbing other installed packages.

Note that the package inputs are still subject to caching. Only the
output directories that would hold links to inputs are not cached.
}


@section{Package Settings}

@defsetting*[XIDEN_ALLOW_UNSUPPORTED_RACKET]{
When true, continue installing when a @tech{package definition}
declares that it does not support the running Racket version.
}

@defsetting*[XIDEN_INSTALL_SOURCES]{
Defines installations in a transaction.

Each list in @racket[XIDEN_INSTALL_SOURCES] consists of three strings:

@itemlist[#:style 'ordered
@item{The path to a symbolic link to create with respect to @racket[(current-directory)].}
@item{The name of a desired output from a @tech{package definition}.}
@item{A URL, file path, or @tech{launcher}-specific string used to find the @tech{package definition}.}
]
}

@defsetting*[XIDEN_INPUT_OVERRIDES]{
A list of strings used to define package input overrides.

Each element is in the form @racket[(cons pattern input-expr)]. The
input of name @racketid[input-name] is replaced with the (evaluated)
@racketid[input-expr] for all @tech{package queries} matching
@racket[pattern].

If @racket[pattern] is a string, then it is used as an argument to
@racket[pregexp] before matching. If @racket[pattern] is a symbol,
then it is first coerced to a string and then used as an argument to
@racket[pregexp].
}

@defsetting*[XIDEN_INSTALL_ABBREVIATED_SOURCES]{
Like @racket[XIDEN_INSTALL_SOURCES], except each item in the list only needs to
be a URL, file path, or @tech{launcher}-specific string used to find the @tech{package
definition}. The symbolic link name is assumed to be the string bound to
@racketfont{package} in the definition, and the output is assumed to be
@racket{default}.
}

@defsetting*[XIDEN_INSTALL_DEFAULT_SOURCES]{
Like @racket[XIDEN_INSTALL_SOURCES], except each list only needs two strings:


@itemlist[#:style 'ordered
@item{The path of a symbolic link to create with respect to @racket[(current-directory)].}
@item{A URL, file path, or @tech{launcher}-specific string used to find the @tech{package definition}.}
]

The output is assumed to be @racket{default}.
}


@section{Package Messages}

@defstruct*[($package $message) () #:prefab]{
A @tech{message} from a package's runtime.
}

@defstruct*[($package:log $package) ([query package-query?]
                                     [output-name string?]
                                     [messages subprogram-log/c]) #:prefab]{
A @tech{message} containing other messages relevant to building a particular package output.
}

@defstruct*[($package:output $package) () #:prefab]{
A @tech{message} pertaining to a package output.
}

@defstruct*[($package:output:built $package:output) () #:prefab]{
Xiden successfully built a package output.
}

@defstruct*[($package:output:reused $package:output) () #:prefab]{
Xiden reused a previously-built package output.
}

@defstruct*[($package:output:undefined $package:output) () #:prefab]{
A requested output is not defined in a corresponding @tech{package
definition}.
}

@defstruct*[($package:unsupported-racket-version $package)
            ([versions racket-version-ranges/c]) #:prefab]{
A @tech{package} claims that the software it builds does not support
the running version of Racket.
}

@defstruct*[($package:unsupported-os $package)
            ([supported (listof symbol?)]) #:prefab]{
A @tech{package} claims that it, or the software it builds, does not support
the current operating system. Supported systems in @racket[supported]
are possible values from @racket[(system-type 'os)].
}

@defstruct*[($package:unavailable-output $package)
            ([available (listof string?)]) #:prefab]{
The @racket[requested] output for a package is not among the @racket[available] outputs.
}
