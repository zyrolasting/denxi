#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Catalogs}

A @deftech{catalog} is a service that maps @tech{package queries} to
@tech{package definitions}, or other data. This way, users do not have
to specify the location of their requested information.


@section{Using a Catalog}

To define a catalog for your @tech{workspace}, open your plugin file
and @racket[provide] one bound to the @racketid[catalog]
identifier. This controls how @racket[catalog-source] behaves. See
@racket[catalog] for information about how to write your own catalog.


@section{Catalogs Are (More) Authoratative}

Catalogs decide what package definitions match package
queries. Beware: Two catalogs may return different answers for the
same query. The same catalog could even opt to return different
answers for the same query at different times. This means Xiden
processes depend on a catalog for expected results.

Ideally, a catalog will always return the same answer for any query
that names an exact @tech{revision}. However, a catalog might also
reserve special revision names for returning the latest revision, or
revisions closest to a specific date.


@section{Trusting Catalogs}

Catalogs may require different levels of trust. Consider a catalog
that hosts package definitions as well as files for package
inputs. Assume it signs every upload with the same private key. That
catalog would be easier to use because you only have to trust one
public key, but it does not help you verify the identity of uploaders
beyond the efforts of the catalog's administator.
