#lang denxi/document

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
in the module body before evaluation, and are never provided from the
module.
}

@defform*[((input name) (input name plinth))]{
Adds a @tech{package input} to @racket[package-inputs]. A form
corresponds exactly to an application of @racket[make-package-input].
}

@defform[(metadatum id value)]{
Adds a string with @racket['id] to @racket[package-metadata].
}

@defform[(output name . body) #:contracts ([name non-empty-string?])]{
Adds a @tech{package output} to @racket[package-outputs].
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
