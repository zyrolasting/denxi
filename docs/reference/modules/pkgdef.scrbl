#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/string
                    denxi/artifact
                    denxi/dig
                    denxi/file
                    denxi/input
                    denxi/subprogram
                    denxi/output
                    denxi/package
                    denxi/pkgdef/static
                    denxi/version
                    @only-in[denxi/url url-string?]
                    @except-in[denxi/pkgdef artifact #%module-begin]]
         @for-syntax[racket/base
                     racket/list
                     racket/match
                     @except-in[denxi/pkgdef artifact #%module-begin]]
         "../../shared.rkt"]

@title{Package Definitions}

@defmodule[denxi/pkgdef]

A @deftech{package definition} is Racket code using the
@racket[PACKAGE_DEFINITION_MODULE_LANG] module language, namely
@racketmodname[denxi/pkgdef]. Since Racket source code may be viewed
as data, package definitions may appear in memory as syntax objects or
lists.

When evaluated, a package definition performs a @deftech{collection
pass} that applies all terms found in @secref{pkgdef-terms} to
@racket[empty-package]. The result is a new @tech{package}, provided
using @racket[(provide pkg)].


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

@defform*[((input name) (input name plinth))]{
Adds a @tech{package input} to @racket[package-inputs]. A form
corresponds exactly to an application of @racket[make-package-input].
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
Adds a @tech{package output} to @racket[package-outputs].

Blank outputs like @racket[(output "name")] are acceptable, but are
only useful when expecting cached outputs.
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


@section{Reprovided Binding Index}

@racketmodname[denxi/pkgdef] reprovides bindings from several other
modules in the collection. They are indexed here for reference.

@(define-for-syntax (fold-ids ids)
   (foldl (lambda (id res)
            (define k (string-ref (symbol->string id) 0))
            (hash-set res k (cons id (hash-ref res k null))))
           (hash)
           ids))

@(define-for-syntax (unfold-ids h)
   (sort (map (lambda (l) (sort l symbol<?)) (hash-values h))
         #:key car
         symbol<?))

@(define-syntax (reprovided stx)
   (define-values (v s) (module->exports 'denxi/pkgdef))
   (define exported-ids (match (append v s) [`((,o (,i ,_ ...) ...) ...) (flatten i)]))
   (define useable-ids (remove '#%module-begin exported-ids))
   (define grouped-ids (unfold-ids (fold-ids useable-ids)))
   (with-syntax ([(group ...) grouped-ids])
     #'(begin (para (reprovided-group group)) ...)))

@(define-syntax (reprovided-group stx)
   (syntax-case stx ()
     [(_ (a)) #'(racket a)]
     [(_ (a . b))
     #'(elem (reprovided-group (a))
             " · "
             (reprovided-group b))]))


@reprovided[]

@include-section{pkgdef/static.scrbl}
