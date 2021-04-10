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
                    xiden/url
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
                     (-> resolved-package-query? (or/c #f source?))]
                     [get-package-definition-chf
                     (-> resolved-package-query? md-algorithm/c)]
                     [get-package-definition-digest-source
                     (-> resolved-package-query? (or/c #f source?))]
                     [get-package-definition-public-key-source
                     (-> resolved-package-query? (or/c #f source?))]
                     [get-package-definition-signature-source
                     (-> resolved-package-query? (or/c #f source?))])]{
A @deftech{catalog} is an instance of @racket[catalog]. Use a catalog
to define @tech{resolved package queries} and @tech{package inputs} in
terms of any @tech{package query}.

A catalog may or may not operate over the network. A catalog is a
naming authority for every name mentioned in a package query. In the
event multiple catalogs conflict on names, the @tech{plugin} decides
which is canonical for its @tech{workspace}.

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
           get-package-definition-source
           _
           _
           _
           _) C)

(define provider (get-default-provider))
(define package (get-default-package provider))
(define edition (get-default-edition provider package))
(define min-revision (get-default-min-revision provider package edition))
(define max-revision (get-default-max-revision provider package edition))
(define interval-bounds (get-default-interval-bounds))

(parsed-package-query provider package edition min-revision max-revision interval-bounds)
]

The fields with identifiers ending in @racketid[*-source] each return
a @tech{source} for the data named in the corresponding identifier, or
@racket[#f] if no source exists. The @tech{resolved package query}
argument for each procedure should be in @tech{canonical form} with
respect to the catalog.

@racket[get-package-definition-chf] returns the cryptographic hash
function (CHF) used to compute the digest for the package definition
found using @racket[get-package-definition-source]. It is reasonable
to expect this to change to reflect a third party's confidence in a
CHF.

@racket[get-default-provider]'s return value may change, but should do
so rarely (if ever). In that context, the default provider is expected
to reflect the catalog as a provider itself. Catalog hosts that expect
to change default providers should therefore instruct clients to
select their own catalog name.
}

@defproc[(get-default-catalog) catalog?]{
Returns a @tech{catalog} that uses @racket[DEFAULT_STRING] as the
default value for every package query field except the interval
bounds.  The interval bounds default to @racket{ii}.

The returned catalog resolves revision numbers and package definitions
under the rules of @racket[set-catalog-http-host] and the current
value of @racket[(XIDEN_DEFAULT_CATALOG_BASE_URL)]. This implies that
two calls to @racket[(get-default-catalog)] will return catalogs that
behave differently if @racket[XIDEN_DEFAULT_CATALOG_BASE_URL] changes
in between.
}


@defproc[(autocomplete-parsed-package-query [cat catalog?]
                                            [query parsed-package-query?])
                                            well-formed-package-query?]{
Replaces all empty strings in @racket[query] using the given
@tech{catalog}. If the catalog opts to use empty strings, then the
output will contain empty strings.
}


@defproc[(add-catalogged-inputs [package package?] [query package-query-variant?] ...) package?]{
Functionally appends inputs to @racket[package]. Each input's name is
exactly that of the corresponding @racket[query], and it includes all
information provided by the @tech{plugin}'s defined catalog. The data
returned by the input is expected to be that of a package definition's
contents.
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



@defproc[(find-input-info [cat catalog?] [query package-query-variant?]) input-info?]{
Returns a @tech{package input} for the package definition found using
the given catalog.

The returned value may or may not pass
@racket[well-formed-input-info/c]. If it does not, the @tech{runtime
configuration} may reject it for a defined reason.
}

@defproc[(find-integrity-info [cat catalog?] [query package-query-variant?]) integrity-info?]{
Returns integrity information for the package definition found using
the given catalog.

If no integrity information is found, then the output value will not
pass @racket[well-formed-integrity-info/c].
}

@defproc[(find-signature-info [cat catalog?] [query package-query-variant?]) signature-info?]{
Returns signature information for the package definition found using
the given catalog.

If no signature information is found, then the output value will not
pass @racket[well-formed-signature-info/c].
}



@deftogether[(
@defproc[(find-package-definition [host catalog?]
                                  [query package-query-variant?])
                                  (or/c #f source?)]
@defproc[(find-package-definition-digest [host catalog?]
                                         [query package-query-variant?])
                                         (or/c #f source?)]
@defproc[(find-package-definition-signature [host catalog?]
                                            [query package-query-variant?])
                                            (or/c #f source?)]
@defproc[(find-package-definition-public-key [host catalog?]
                                             [query package-query-variant?])
                                             (or/c #f source?)]
@defproc[(find-package-definition-chf [host catalog?]
                                      [query package-query-variant?])
                                      md-algorithm/c]
)]{
Use these procedures to learn about a @tech{package definition} on a
@tech{catalog}.

The procedures that may return a @tech{source} are expected to produce
bytes for the content named by each procedure, or @racket[#f] if no
source is found. Each procedure uses a corresponding accessor field in
@racket[catalog], and will coerce @racket[query] to @tech{canonical
form} with respect to @racket[host].

@racket[find-package-definition-chf] is different only in that it must
return a non-@racket[#f] value.

None of these procedures force a complete revision interval. If you
wish to do this, bind @racket[query] to the output of
@racket[make-canonical-package-query], where
@racket[#:force-complete-interval?] is a true value.
}


@section{Built-in Catalog Variants}

@defproc[(set-catalog-filesystem-directory [cat catalog?]
                                           [directory-path complete-path?]) catalog?]{
Returns a new catalog that handles revision numbers and package
definitions in terms of a filesystem directory.

The returned catalog uses the existing
@racket[catalog-get-package-definition-chf] implementation in
@racket[cat], so be sure it does what you expect in terms of the rules
below.

For a given provider @litchar{P}, package name @litchar{K}, edition
@litchar{E}, revision name @litchar{N}, and revision number
@litchar{#}, the following invariants must hold:

@itemlist[

@item{
@racket[directory-path] and all contents must be readable.
}

@item{
@litchar{K} is not @racket[equal?] to @racket{public-key}.
}

@item{
@racket[file-name-string?] returns @racket[#t] for all values of
@litchar{P}, @litchar{K}, @litchar{E}, @litchar{N}, and @litchar{#}.
}

@item{
A package definition is located at @litchar{P/K/E/#}
w.r.t. @racket[directory-path].
}

@item{
Digests are located at @litchar{P/K/E/#.CHF}, where @racketid[CHF] is
the string name of the cryptographic hash function returned from
@racket[catalog-get-package-definition-chf].
(e.g. @racket{acme/anvil/heavy/2.sha1}).
}

@item{
Signatures are located at @litchar{P/K/E/#.CHF.sig}.
}

@item{
All @tech{revision names} must be encoded as symbolic links pointing
to package definition files. Each such link must be located at
@litchar{P/K/E/N}. Digest and signature links are not required.
}

@item{
@racket[(build-path directory-path P ".public-key")] must exist as a
readable public key for some provider @racketid[P].
}

]

In terms of the above, here is a valid directory structure for a
catalog.

@verbatim{
~/xiden-catalog
├── alice
│   ├── default -> renderer
│   ├── public-key
│   ├── raw-input
│   │   └── default
│   │       ├── 0
│   │       ├── 0.sha3-384
│   │       ├── 0.sha3-384.sig
│   │       ├── 1
│   │       ├── 1.sha3-384
│   │       ├── 1.sha3-384.sig
│   │       ├── 5
│   │       ├── 5.sha3-384
│   │       ├── 5.sha3-384.sig
│   │       ├── open-beta -> 5
│   │       └── xbox-controller-support -> 1
│   └── renderer
│       ├── default -> vulkan
│       ├── directx
│       │   ├── 0
│       │   ├── 0.sha3-384
│       │   └── 0.sha3-384.sig
│       └── vulkan
│           ├── 0
│           ├── 0.sha3-384
│           ├── 0.sha3-384.sig
│           └── default -> 0
├── default -> alice
└── john
    ├── calculator
    │   ├── default
    │   │   ├── 0
    │   │   ├── 0.sha3-384
    │   │   ├── 0.sha3-384.sig
    │   │   ├── 1
    │   │   ├── 1.sha3-384
    │   │   ├── 1.sha3-384.sig
    │   │   ├── 2
    │   │   ├── 2.sha3-384
    │   │   ├── 2.sha3-384.sig
    │   │   ├── initial -> 0
    │   │   └── post-feedback -> 2
    │   └── scientific
    │       ├── 0
    │       ├── 0.sha3-384
    │       ├── 0.sha3-384.sig
    │       ├── 1
    │       ├── 1.sha3-384
    │       ├── 1.sha3-384.sig
    │       └── zero-day-patch -> 1
    ├── calendar
    │   ├── chinese
    │   │   ├── 0
    │   │   ├── 0.sha1
    │   │   └── 0.sha1.sig
    │   └── gregorian
    │       ├── 0
    │       ├── 0.sha1
    │       └── 0.sha1.sig
    └── public-key
}

In this example, the providers are Alice, a video game developer, and
John, an office application developer.  Note that the entries contain
a mix of @litchar{sha1} and @litchar{sha3-384} digests, which speaks
to the responsibility of @racket[catalog-get-package-definition-chf]
in some cases.

There is a gap in revision numbers in Alice's @tt{raw-input} package.
This is fine, because it reflects a provider's need to retract
releases. The catalog will return the latest available match for
queries like @litchar{alice:raw-input:::open-beta}.

Finally, each directory may contain a symlink named after a value
returned by a @racketid[catalog-get-default-*] accessor. You can
leverage this to support getting the latest available version using
queries like @litchar{alice:raw-input}. In fact, this example resolves
the empty package query to @litchar{alice:renderer:vulkan:0:0:ii}
when using the value of @racket[(get-default-catalog)].
}


@defproc[(set-catalog-http-host [cat catalog?]
                                [base-url-variant (or/c url? url-string?)])
                                catalog?]{
Returns a new catalog that handles revision numbers and package
definitions in terms of an HTTP server. HTTPS is enabled if
@racket[base-url-variant] uses the @racket{https://} scheme, and will
use the latest TLS or SSL implementation available.

A web server that serves files from a directory suitable for use with
@racket[set-catalog-filesystem-directory] is in turn suitable for use
with @racket[set-catalog-http-host], provided the server resolves
symbolic links.

The catalog will only issue @tt{GET} requests
w.r.t. @racket[base-url-variant], expecting only a response with a
@tt{200} status and a body for successful fulfilment.

The returned catalog uses the existing
@racket[catalog-get-package-definition-chf] implementation.

For a given package definition with a provider @litchar{P}, a package
name @litchar{K}, an edition @litchar{E}, a revision name @litchar{N},
a revision number @litchar{#}, and a cryptographic hash function name
@tt{CHF}, the following invariants must hold for expected response
bodies.

@itemlist[

@item{
@litchar{GET /P/K/E/#} or @litchar{GET /P/K/E/N} yields a package
definition, using @litchar{Content-Type: application/octet-stream} or
@litchar{Content-Type: text/plain}.
}

@item{
@litchar{GET /P/public-key} yields a public key, using
@litchar{Content-Type: application/octet-stream} or
@litchar{Content-Type: text/plain}.
}

@item{
@litchar{GET /P/K/E/N.CHF} yields a digest's raw bytes, using
@litchar{Content-Type: application/octet-stream}.
}

@item{
@litchar{GET /P/K/E/N.CHF.sig} yields the signature bytes for a digest, using
@litchar{Content-Type: application/octet-stream}.
}

]

All requests are fulfilled through @racket[fetch], meaning response
bodies are cached accordingly. Run @litchar{xiden gc} before using
this kind of catalog to force local cache misses.
}
