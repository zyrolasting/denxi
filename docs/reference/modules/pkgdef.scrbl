#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/string
                    xiden/file
                    xiden/input-info
                    xiden/logged
                    xiden/package
                    xiden/rc
                    @only-in[xiden/url url-string?]
                    @except-in[xiden/pkgdef #%module-begin]]
         "../../shared.rkt"]

@title{Package Definitions}

@defmodule[xiden/pkgdef]

@racketmodname[xiden/pkgdef] is a functional module language for
writing @tech{package definitions}. It expands using a
@deftech{collection pass} for the terms defined in
@secref{pkgdef-terms}. Those terms apply to an instance of
@racket[package], starting from @racket[empty-package].  The result is
provided from the module as @racket[pkg].


@section[#:tag "pkgdef-terms"]{Package Definition Terms}

@defform*[((define id value)
           (define (id formals ...) body ...))]{
@racket[define] is allowed in module context to bind procedures and
common values. All @racket[define]s are hoisted above all other terms
before evaluation.
}


@defform[(description string-fragment ...) #:contracts ([string-fragment non-empty-string?])]{
Sets @racket[package-description].

This form will gather all string fragments into a single string, so
that you can divide up the message in source code.

Note that the string fragments are concatenated as-is, so take care
with whitespace placement.

@racketblock[
(description "This is a "
             "string that will"
             " appear as one line.")
]
}

@defform[(edition str) #:contracts ([str non-empty-string?])]{
Sets @racket[package-edition].
}

@defform*[((input name) (input name integrity) (input name integrity signature))]{
Adds a @tech{package input} to @racket[package-inputs]. A form
corresponds exactly to an application of @racket[make-input-info].
}

@defform[(metadatum id value)]{
Adds a string with @racket['id] to @racket[package-metadata].
}

@defform[(name str)
         #:contracts ([str non-empty-string?])]{
Sets @racket[package-name].
}

@defform[(os-support os ...)]{
Sets @racket[package-os-support].

Each @racket[os] must be a possible value of @racket[(system-type
'os)].
}

@defform[(output name body ...)
         #:contracts ([name non-empty-string?])]{
Defines a @deftech{package output}, which is a named subprogram where
@racket[body] sits in an implicit @racket[mdo] form.  The output is
encoded as part of the implementation of @racket[package-build].
The name is added to @racket[package-output-names].
}

@defform[(provider str)
         #:contracts ([str non-empty-string?])]{
Sets @racket[package-provider].
}

@defform[(racket-versions supported ...)
          #:grammar [(supported (min-version max-version)
                                exact-version)]]{
Sets @racket[package-racket-versions].

Each @racket[supported] subform may be an inclusive Racket version
range or an exact Racket version, e.g. @racket[(racket-versions ("6.0"
"7.7.0.5") "5.4")].

You may replace any version string with @racket{*} to remove a bound.
This way, @racket[(racket-versions ("6.0" "*"))] represents all
versions above and including 6.0. If the version string is not
@racket{*}, it must be a @racket[valid-version?].
}

@defform[(revision-names str ...) #:contracts ([str non-empty-string?])]{
Sets @racket[package-revision-names].
}

@defform[(revision-number num) #:contracts ([num revision-number?])]{
Sets @racket[package-revision-number].
}

@defform[(tags t ...) #:contracts ([t non-empty-string?])]{
Sets @racket[package-tags].
}

@defform[(url location) #:contracts ([location url-string?])]{
Sets @racket[package-url].
}


@section{Additional Bindings}

@(define-syntax-rule (reprovided sym ...)
  (para (racketmodname xiden/pkgdef)
  " reprovides the following bindings for use in "
  (tech "package outputs") " and procedure bodies "
  (elem (racket sym) " ") ...))

@(reprovided #%app
             #%datum
             :=
             base32
             base64
             coerce-source
             define
             description
             edition
             extract
             file-source
             find-input
             from-catalogs
             from-file
             hex
             http-mirrors-source
             http-source
             in-paths
             input-ref
             input
             integrity
             install
             lines-source
             mdo
             metadatum
             name
             os-support
             output
             plugin-source
             provider
             quote
             racket-versions
             release-input
             resolve-input
             revision-names
             revision-number
             run
             signature
             sources
             tags
             text-source
             url)

@include-section{pkgdef/static.scrbl}
