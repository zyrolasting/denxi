#lang scribble/manual

@require["../../shared.rkt"
         @for-label[racket/base
                    racket/contract
                    racket/match
                    racket/string
                    xiden/catalog
                    xiden/message
                    xiden/query
                    xiden/source
                    xiden/string
                    xiden/version]]

@title{Catalogs}

@defmodule[xiden/catalog]

@defstruct*[catalog ([get-default-provider
                     (-> string?)]
                     [get-default-package
                     (-> string? string?)]
                     [get-default-edition
                     (-> string? string? string?)]
                     [get-default-min-revision
                     (-> string? string? string? string?)]
                     [get-default-max-revision
                     (-> string? string? string? string?)]
                     [get-default-interval-bounds
                     (-> string?)]
                     [get-revision-number
                     (-> string?
                         string?
                         string?
                         string?
                         (or/c #f revision-number-variant?))]
                     [get-package-definition-source
                     (-> resolved-package-query? source?)])]{
A @deftech{catalog} is an instance of @racket[catalog]. A catalog maps
@tech{package queries} to @tech{package definitions} and defines
default values for use in package queries.

A catalog may or may not operate over the network. A catalog is
considered a naming authority for every name mentioned in a package
query. In the event multiple catalogs conflict on names, the winner is
determined by the @tech{runtime configuration}.

The fields related to package queries are used together to
progressively compute default values for a
@racket[parsed-package-query]. The following program will construct a
default query suitable for use with any catalog @racketid[C].

@racketblock[
(match-define
  (catalog get-default-provider
           get-default-package
           get-default-edition
           get-default-min-revision
           get-default-max-revision
           get-default-interval-bounds
           get-revision-number
           get-package-definition-source) C)

(define provider (get-default-provider))
(define package (get-default-package provider))
(define edition (get-default-edition provider package))
(define min-revision (get-default-min-revision provider package edition))
(define max-revision (get-default-max-revision provider package edition))
(define interval-bounds (get-default-interval-bounds))

(parsed-package-query provider package edition min-revision max-revision interval-bounds)
]

@racket[get-package-definition-source] is used to return a
@tech{source} for a package definition. The argument should be in
@tech{canonical form} with respect to the corresponding instance.

@racket[get-default-provider]'s return value may change, but should do
so rarely (if ever) if the catalog is meant for use in
@racket[catalog-source]. In that context, the default provider is
expected to reflect the catalog as a provider itself. Catalog hosts
that expect to change default providers should therefore instruct
clients to select their own catalog name.
}

@defthing[default-catalog catalog?]{
A @tech{catalog} that uses @racket[DEFAULT_STRING] as the default
value for every package query field except the interval bounds.
The interval bounds default to @racket{ii}.

Revision numbers and package definitions are resolved under the rules
of @racket[set-catalog-filesystem-directory], where the directory
is @racket[(build-path (find-system-path 'home-dir) "xiden-catalog")].
}

@defstruct[catalog-source ([query package-query?])]{
A @tech{source} that consults the @tech{plugin}'s @racket[catalog]
using @racket[query].

When used with @racket[identify], a @racket[catalog-source] produces a
value in terms of @racket[(catalog-get-default-provider)] and the
query. Note that two catalogs that conflict on both will cause
unwanted cache hits if combined for use in a @tech{workspace}.
Additionally, catalogs that change their default providers will induce
cache misses for all clients.

You can prevent cache-related surprises by defining a constant default
provider name in your @tech{plugin}.
}

@defproc[(autocomplete-parsed-package-query [cat catalog?]
                                            [query parsed-package-query?])
                                            well-formed-package-query?]{
Replaces all empty strings in @racket[query] using the given
@tech{catalog}. If the catalog opts to use empty strings, then the
output will contain empty strings.
}


@defproc[(make-canonical-package-query
         [#:force-complete-interval? force-complete-interval? any/c #f]
         [cat catalog?]
         [query parsed-package-query?])
         (or/c #f resolved-package-query?)]{
Returns a @tech{resolved package query} based on @racket[query] in
terms of the catalog, or @racket[#f] on failure to do so. A query
returned from @racket[make-canonical-package-query] is said to be in
@deftech{canonical form} with respect to @racketid[cat].

@racket[make-canonical-package-query] first uses
@racket[(autocomplete-parsed-package-query cat query)], then converts
the @tech{revision names} in the result to @tech{revision numbers}
using @racket[catalog-get-revision-number] and
@racket[resolve-revision-interval].

In the event only one of the two revision interval endpoints is
resolved, the return value depends on
@racket[force-complete-interval?].  When @racket[#t], the sole
resolved revision number is used for both ends of revision interval in
the return value, making it an @tech{exact package query}. Note that
this reasoning applies for either the minimum or maximum revision, so
@italic{only set @racket[force-complete-interval?] to @racket[#t]
when the minimum and maximum revisions are considered equivalent.}

When @racket[force-complete-interval?] is @racket[#f], then
@racket[make-canonical-package-query] returns @racket[#f] when only
one revision is resolved.
}


@defproc[(find-package-definition [host catalog?]
                                  [query package-query-variant?])
                                  (or/c #f source?)]{
Returns a @tech{source} expected to produce a @tech{package
definition}, or @racket[#f] if no source is found. 

The return value comes from a call to
@racket[(catalog-get-package-definition-source host)].

If necessary, @racket[find-package-definition] will coerce
@racket[query] to @tech{canonical form} with respect to @racket[host]
before passing it to @racket[(catalog-get-package-definition-source
host)]. It will @italic{not} force a complete interval in doing so. If
you wish to control this, pass the output of
@racket[make-canonical-package-query] as an argument with your chosen
value for @racket[#:force-complete-interval?].
}


@section{Built-in Catalog Variants}

@defproc[(set-catalog-filesystem-directory [cat catalog?] [directory-path complete-path?]) catalog?]{
Returns a new catalog that handles revision numbers and package
definitions in terms of a filesystem directory.

The following invariants must hold:

@itemlist[

@item{
@racket[directory-path] and all contents must be readable for a Xiden
process.
}

@item{
A package definition's provider name, package name, edition, and all
revision names must match @racket[file-name-string?].
}

@item{
For a given package definition with a provider @litchar{P}, a package
name @litchar{K}, an edition @litchar{E} and a revision number
@litchar{N}, the file for that package definition must be
located at @litchar{P/K/E/N.rkt} w.r.t. @racket[directory-path].
}

@item{
All @tech{revision names} must be encoded as symbolic links pointing
to package definition files. Each such link must be in the same
directory as the package definition file it references.
}
]
}
