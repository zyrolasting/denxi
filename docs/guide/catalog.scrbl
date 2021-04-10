#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base xiden/catalog]]

@title{Catalogs}

A @deftech{catalog} translates @tech{package queries} to package
definitions, signatures, integrity information, or more specific
package queries. Each catalog acts as a source of social conventions
and meaning that Xiden itself does not offer.

To define a catalog for your @tech{workspace}, open your plugin file
and @racket[provide] an instance of @racket[catalog] bound to the
@racketid[canonical-catalog] identifier. Or, do nothing, in which case
you'll use a catalog returned from @racket[(get-default-catalog)].

Beware: Two catalogs may disagree about what a package query
means. The same catalog could even opt to return different answers for
the same query at different times. The plugin decides which answers
win, and is able to combine the catalogs in a way that makes sense for
the @tech{workspace}.

Catalogs may require different levels of trust. Consider a catalog
that signs every package definition with the same private key. That
catalog would be easier to use because you only have to trust one
public key, but it does not help you verify the identity of providers
beyond the efforts of the catalog's administator.
