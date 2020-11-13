#lang scribble/manual

@require[@for-label[racket/base racket/contract racket/string xiden/query xiden/version]
          "../../shared.rkt"]

@title{Package Queries}

@defmodule[xiden/query]

A @deftech{package query} is a colon-separated string that matches
against the discovery information in @tech{package definitions}.  Use
package queries to define @tech{version} intervals.


@section{Package Query Syntax}

With no omissions, a query follows this form:

@verbatim[#:indent 4]|{
<provider>:<package>:<edition>:<revision-min>:<revision-max>:<interval-bounds>
}|

e.g. @racket{example.com:htdp:teachers:8:201:ii}

@tt{<provider>} is, as expected, the name of the package's provider.
This may or may not be the author of the package.  A domain name for a
provider name is not required, but using one is helpful if your
package appears across multiple services.

@tt{<package>} is the name of the package.

@tt{<revision-min>} and @tt{<revision-max>} can each be a
@tech{revision number} or a @tech{revision name}.

Finally, @tt{<interval-bounds>} can be @racket{ee}, @racket{ie},
@racket{ei}, or @racket{ii}. These characters control if
@tt{<revision-min>} or @tt{<revision-max>} are interpreted as
inclusive or exclusive interval boundaries. The first character
controls @tt{<revision-min>}. The second character controls
@tt{<revision-max>}. If the character is @racket{i}, then the boundary
is inclusive of the set revision. If @racket{e}, exclusive.


@section{Package Query Omissions}

To omit a field, use consecutive colons, or leave fields off the end
of the string. An omitted field is set to the empty string.

e.g. @racket{example.com:htdp::8::ie}, or @racket{example.com:htdp}.

When a field is set to the empty string, a fallback value may be used
instead. This manual does not define a standard for default values
that other services have to follow. However, if this manual mentions a
default value for information that appears as a package query's field
(such as an @tech{edition}), then that is the default value for the
field in that context.

The empty string is equivalent to accepting only default values.


@section[#:tag "pkg-query-classifications"]{Package Query Classifications}

A @deftech{parsed package query} is an instance of @racket[xiden-query].

A @deftech{well-formed package query} is a @tech{parsed package query}
that uses non-empty strings for package and provider names, and has
strings for all other fields.

A @deftech{malformed package query} is a @tech{parsed package query}
that is not a @tech{well-formed package query}.

A @deftech{resolved package query} is a @tech{well-formed package
query} where the minimum and maximum @tech{revisions} are digit
strings that can be trivially converted to @tech{revision numbers}.

An @deftech{exact package query} is a @tech{resolved package query}
where the minimum and maximum @tech{revisions} form an inclusive
interval that matches exactly one @tech{version}.


@section{Parsing a Package Query}

Assume that a query @racketid[Q] is bound to a @tech{well-formed
package query} with no omissions. In that case you can construct an
instance of @racket[xiden-query] using @racket[(apply xiden-query
(string-split Q ":"))].

In the general case where fields may be omitted, any missing fields
should be set to the empty string like so:

@racketblock[
(define (string->xiden-query s)
  (define user-defined (string-split s ":"))
  (define num-fields (length user-defined))
  (apply xiden-query
         (build-list (procedure-arity xiden-query)
                     (λ (i)
                       (if (< i num-fields)
                           (list-ref user-defined i)
                           "")))))
]


@section{Package Query API}

@defstruct*[xiden-query ([provider-name string?]
                         [package-name string?]
                         [edition-name string?]
                         [revision-min string?]
                         [revision-max string?]
                         [interval-bounds string?])]{
A parsed form of a @tech{package query}.
}

@deftogether[(
@defproc[(well-formed-xiden-query? [v any/c]) boolean?]
@defproc[(malformed-xiden-query? [v any/c]) boolean?]
@defproc[(resolved-xiden-query? [v any/c]) boolean?]
@defproc[(exact-xiden-query? [v any/c]) boolean?]
)]{
Each procedure returns @racket[#t] if @racket[v] matches the
respective rule in @secref{pkg-query-classifications}.
}

@defthing[xiden-query-string? predicate/c]{
Returns @racket[#t] if the input value is suitable for use as an
argument to @racket[string->xiden-query].
}


@defthing[xiden-query-variant? predicate/c]{
Returns @racket[#t] if the input value is suitable for use as an
argument to @racket[coerce-xiden-query]. Specifically, the input can
be a string, an instance of @racket[xiden-query], or a procedure that
behaves like an evaluator for a @racketmodname[xiden/pkgdef] module.
}

@defthing[boundary-flags-string? predicate/c]{
Returns @racket[#t] if the argument is a suitable value for
@racket[xiden-query-interval-bounds].
}


@defproc[(coerce-xiden-query [variant xiden-query-variant?]) xiden-query?]{
Returns a @racket[xiden-query] from a variant value type.
}

@defproc[(xiden-query->string [query well-formed-xiden-query?]) string?]{
Converts a @racket[xiden-query] to a string, with no validation
performed on the fields in advance. The fields set in @racket[query]
are joined into the string as-is.
}

@defproc[(package-evaluator->xiden-query [pkgeval (-> any/c any)]) xiden-query?]{
Returns an instance of @racket[xiden-query], such that the fields
are populated from @racket[pkgeval].

@racket[pkgeval] is an evaluator for a @racketmodname[xiden/pkgdef]
module, or a procedure that acts like one.
}

@defproc[(resolve-revision-interval [query xiden-query?]
                                    [make-revision-number
                                     (-> boolean? string? revision-number?)])
         (values revision-number? revision-number?)]{
Returns a value computed from @racket[make-revision-interval].  The
arguments for @racket[make-revision-interval] is based on the
information in @racket[query], such that each revision endpoint is
first transformed to a @tech{revision number} using
@racket[make-revision-number].

@racket[make-revision-number] accepts two arguments.

If the first argument is @racket[#f], then the second argument is
the same value bound to @racket[(xiden-query-revision-min query)].

If the first argument is @racket[#t], then the second argument is
the same value bound to @racket[(xiden-query-revision-max query)].

The first argument is useful for generating mock data for tests, but
is otherwise unhelpful when normalizing an arbitrary @tech{revision}
to a @tech{revision number}.
}

@defproc[(abbreviate-exact-xiden-query [query exact-xiden-query?]) string?]{
Like @racket[xiden-query->string], except the resulting string omits
more fields. Since @racket[query] is exact, this does not result in
lost information.
}

@defproc[(string->xiden-query [str string?]) xiden-query?]{
Converns a string to an instance of @racket[xiden-query].
}
