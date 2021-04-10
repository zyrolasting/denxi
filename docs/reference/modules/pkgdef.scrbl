#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/string
                    xiden/catalog
                    xiden/file
                    xiden/input-info
                    xiden/logged
                    xiden/package
                    xiden/query
                    xiden/rc
                    xiden/version
                    @only-in[xiden/url url-string?]
                    @except-in[xiden/pkgdef #%module-begin]]
         @for-syntax[racket/base
                     racket/list
                     racket/match
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

@defform[(comission anything ...)]{
Comissions a new package from the installed @tech{plugin}.

@racket[comission] expands to @racket[(override-package P anything ...)],
where @racket[override-package] is defined by the corresponding
@tech{plugin}, and @racketid[P] is the current state of the package
during evaluation of terms.

If @racket[override-package] is not defined, then @racket[comission]
has no effect. Any package definition using @racket[comission] might
not function without a compatible plugin installed in the
@tech{workspace}.

@racketmod[xiden
(code:comment "This example invokes the plugin during evaluation")
(code:comment "The plugin will see the provider, package, and edition name, but not the revision.")
(provider "example.com")
(package "anvil")
(edition "heavy")
(comission)
(revision-number 1)
]

@racketmod[xiden
(code:comment "This example invokes the plugin with arguments")
(comission "anvil" #:verbose? #t)
]
}

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


@section{Reprovided Binding Index}

@racketmodname[xiden/pkgdef] reprovides bindings from several other
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
   (define-values (v s) (module->exports 'xiden/pkgdef))
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
