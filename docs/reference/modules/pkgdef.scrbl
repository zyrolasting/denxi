#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    xiden/logged
                    xiden/package
                    xiden/rc
                    @except-in[xiden/pkgdef do #%module-begin]]
         "../../shared.rkt"]

@title{Module Language}

@defmodule[xiden/pkgdef]

@racketmodname[xiden/pkgdef] is a functional language for writing
@tech{package definitions}.

@racketmodname[xiden/pkgdef] expands by first performing a
@deftech{collection pass} for information about @tech{package inputs},
@tech{package outputs}, @tech{actions}, and user metadata. Once
collected, the aggregate information is placed in a new Racket module.

Some errors are caught during the @tech{collection pass}, such as
backwards Racket version intervals or incorrect value types in
top-level forms. Contract violations for procedure applications are
raised on module instantiation. Given an uncompromised installation,
module instantiation will have no side effect on the file system or
shared program state.

This section will be confusing without understanding the existence of
the @tech{collection pass}, because the top-level forms used to write
package definitions will share identifiers with provided bindings.


@section[#:tag "pkgdef-top-level-forms"]{Top Level Forms for Collection Pass}

@defform[(provider non-empty-string)
         #:contracts ([name non-empty-string?])]{
Defines the name of the provider.
}

@defform[(package name)
         #:contracts ([name non-empty-string?])]{
Defines the name of the package.
}

@deftogether[(
@defform[(edition name)
         #:contracts ([name non-empty-string?])]
@defform[(revision-number num)
         #:contracts ([num revision-number?])]
@defform[(revision-names name ...)
         #:contracts ([name non-empty-string?])]
)]{
Defines the version.
}


@defform[(racket-versions supported ...)
          #:grammar [(supported (min-version max-version)
	                        exact-version)]]{
Defines a set of supported Racket versions for Racket programs
produced by a corresponding @tech{package}.

Each @racket[supported] form may be an inclusive Racket version range
or an exact Racket version, e.g. @racket[(racket-versions ("6.0"
"7.7.0.5") "5.4")].

You may replace any version string with @racket{*} to remove a bound.
This way, @racket[(racket-versions ("6.0" "*"))] represents all
versions above and including 6.0. If the version string is not
@racket{*}, it must be a @racket[valid-version?].
}


@defform[(os-support os ...)]{
Declares OS support. Each @racket[os] must be a possible value of
@racket[(system-type 'os)].

If left out, a package definition assumes support for every operating
system Racket supports.
}


@defform[(metadatum id value)]{
Binds a datum to @racket['id] in a hash table provided by the package
definition.
}

@defform[(input _ ...)]{
Declares a @tech{package input}. Corresponds exactly to
valid applications of @racket[input] in the context of
@racketmodname[xiden/input-info]. The @tech{collection pass}
aggregates these expressions into a provided list.
}

@defform[(action (id formals ...) body ...)]{
An @deftech{action} is a Racket procedure with a body expressed in a
Haskell-esque @tt{do} notation. @tech{Package outputs} may apply
actions to reuse functionality.
}

@defform[(output name body ...)
         #:contracts ([name non-empty-string?])]{
Declares a @tech{package output}. A package output is a named
instruction sequence expressed in a Haskell-esque @tt{do} notation.
}


@section[#:tag "pkgdef-provided-bindings"]{Provided Package Definition Bindings}

The following bindings are provided from every package definition
module, regardless of specific user content.

@defthing[provider non-empty-string?]{
The name of the package's provider.

If not defined by the user, this is bound to @racket{default}.
}

@defthing[package non-empty-string?]{
The name of the package definition, and the packages created from it.

If not defined by the user, this is bound to @racket{default}.
}

@deftogether[(
@defthing[edition non-empty-string?]
@defthing[revision-number revision-number?]
@defthing[revision-names (listof non-empty-string?)]
)]{
Version information.

If @racketid[edition] is not defined by the user, it is bound to @racket{default}.
}

@defthing[metadata (hash/c symbol? any/c)]{
User-defined metadata.
}

@defthing[url url-string?]{
A URL one can use to find additional information.
}

@defthing[output-names (listof non-empty-string?)]{
Available package outputs. This is also the domain for the provided
@racket[build] procedure.
}

@defthing[description string?]{
A human-readable purpose statement.
}

@defthing[tags (listof non-empty-string?)]{
Tags used for discovery information.
}

@defproc[(build [output-name non-empty-string?]) (or/c #f procedure?)]{
If @racket[output-name] is not defined in the @tech{package
definition}, then @racket[build] will simply return @racket[#f].

Otherwise, @racketid[build] returns a procedure @racketid[P] called
for its effect.  @racketid[P] will create files in
@racket[(current-directory)] with the goal of creating the
corresponding output.
}

@defthing[inputs (listof well-formed-input-info/c)]{
Available @tech{package inputs}.
}

@section{Additional Bindings}

@racketmodname[xiden/pkgdef] provides the following bindings.

@itemlist[
@item{@racket[#%top-interaction]}
@item{@racket[base32]}
@item{@racket[base64]}
@item{@racket[call-with-input]}
@item{@racket[from-catalogs]}
@item{@racket[from-file]}
@item{@racket[hex]}
@item{@racket[input]}
@item{@racket[input-ref]}
@item{@racket[integrity]}
@item{@racket[signature]}
@item{@racket[run]}
@item{@racket[install]}
@item{@racket[sources]}
@item{@racket[unpack]}
]

