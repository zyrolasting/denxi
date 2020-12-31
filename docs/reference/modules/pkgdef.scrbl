#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    xiden/file
                    xiden/input-info
                    xiden/logged
                    xiden/package
                    xiden/rc
                    @except-in[xiden/pkgdef #%module-begin]]
         "../../shared.rkt"]

@title{Package Definitions}

@section{Module Language}

@defmodule[xiden/pkgdef]

A @deftech{package definition} is a program written in
@racketmodname[xiden/pkgdef], a functional module language.

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

This section shows how the top-level forms used to write
package definitions turns into provided bindings. In most
cases, data are provided using the same identifier found
in the source code.

@racketinput[
(module pkgdef xiden/pkgdef (package "my-package"))
(require 'pkgdef)
package]
@racketresult["my-package"]

Assume that this is the default behavior unless stated otherwise.


@section[#:tag "pkgdef-top-level-forms"]{Top Level Forms}

@defform[(provider non-empty-string)
         #:contracts ([name non-empty-string?])]{
Defines the name of the package provider.

If omitted, then the provided @racketid[provider] identifier is bound to @racket{default}.
}

@defform[(package name)
         #:contracts ([name non-empty-string?])]{
Defines the name of the package.

If omitted, then the provided @racketid[package] identifier is bound to @racket{default}.
}


@defform[(url location) #:contracts ([location url-string?])]{
The @bold{primary} URL associated with the software, as opposed to
auxillary URLs that can be used with @racket[metadatum].

If omitted, @racket[url] is bound to the empty string.
}

@defform[(description string-fragment ...) #:contracts ([string-fragment non-empty-string?])]{
A summary of the package's functionality. This form will gather all
string fragments into a single string, so that you can divide up the
message in source code.

Note that the string fragments are concatenated as-is, so take care
with whitespace placement.

@racketblock[
(description "This is a "
             "string that will"
             " appear as one line.")
]

If omitted, @racket[description] is bound to the empty string.
}

@defform[(tags non-empty-string ...)]{
Defines short strings used to annotate the package with topics of
interest to users.

@racketblock[
(tags "package management" "dependencies" "functional")
]

If omitted, @racket[tags] is bound to the empty list.
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

If omitted, the @racket[edition] is bound to @racket{default}, the
revision number is bound to @racket[0], and @racket[revision-names] is
bound to the empty list.
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

If omitted, the resulting package definition claims to support every
operating system Racket supports.
}


@defform[(metadatum id value)]{
Binds a datum to @racket['id] in a hash table. That hash table is
provided as @racketid[metadata] by the package definition.

@racketinput[
(module pkgdef xiden/pkgdef (metadatum my-email "email@example.com") (metadatum bitcoin-wallet "adf..."))
(require 'pkgdef)
metadata]

@racketresult['#hasheq((bitcoin-wallet . "adf...") (my-email . "email@example.com"))]

}

@defform[(input _ ...)]{
Declares a @tech{package input}. Corresponds exactly to
valid applications of @racket[input] in the context of
@racketmodname[xiden/input-info].

The @tech{collection pass} aggregates these expressions and
provides them as a single list bound to @racketid[inputs].

@racketinput[
(module pkgdef xiden/pkgdef (input "a") (input "b"))
(require 'pkgdef)
inputs]
@racketresult[
'(#s(input-info "b" () #f #f) #s(input-info "a" () #f #f))]

}

@defform[(action (id formals ...) body ...)]{
An @deftech{action} is a Racket procedure where @racket[body] sits in
an implicit @racket[mdo] form. @tech{Package outputs} may apply
actions to reuse functionality.

Actions are not provided by the package definition. Contracts prevent
their use everywhere except in the body of @tech{package outputs}.
}

@defform[(output name body ...)
         #:contracts ([name non-empty-string?])]{
Declares a @deftech{package output}, which is a named subprogram where
@racket[body] sits in an implicit @racket[mdo] form.

The @tech{collection pass} gathers @tech{package outputs} such that
the resulting Racket module provides a list of all output names bound
to @racketid[output-names], and a @racketid[build] procedure.  The
domain of @racket[build] is the set of elements in
@racket[output-names].  The codomain is a set of @tech{logged
procedures} that each fulfil their respectively named output.

@racketinput[
(module pkgdef xiden/pkgdef
        (input "code.tgz")
        (output "default"
                archive := (input-ref "code.tgz")
                (extract archive)))]
@racketinput[(require 'pkgdef)]
@racketinput[output-names]
@racketresult['("default")]
@racketinput[(logged? (build "default"))]
@racketresult[#t]

As an aside: Do not apply @racket[run-log] to any value computed from
@racketid[build]. Doing so will perform installation steps on your
system! Also, it will not update the database in any proximate
@tech{workspace}, and it will not run in a sandbox.  The option is
available as a way to programmatically test output installation, but
take care to only use @racket[install] or the command line interface
in most cases.
}


@section{Additional Bindings}

@defproc[(input-ref [name string?]) (logged/c input-info?)]{
Returns a @tech{logged procedure} that searches for an input with the
given @racket[name] in the enclosing package definition. Fails if
the input is not defined.

@racketmodname[xiden/pkgdef] generates an @racket[input-ref] for each
module.
}

@(define-syntax-rule (reprovided sym ...)
  (para (racketmodname xiden/pkgdef) " reprovides the following bindings: "
  (elem (racket sym) " ") ...))

@(reprovided
         #%app
         #%datum
         #%top-interaction
         :=
         all-defined-out
         base32
         base64
         begin
         case
         current-directory
         define
         define-values
         else
         except-out
         extract
         find-input
         from-catalogs
         from-file
         hex
         in-paths
         input
         integrity
         list
         logged-unit
         make-immutable-hasheq
         mdo
         provide
         quote
         release-input
         resolve-input
         run
         signature
         sources
         void)

@include-section{pkgdef/static.scrbl}
