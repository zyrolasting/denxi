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
is inclusive of the defined revision. If @racket{e}, exclusive.


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

A @deftech{parsed package query} is an instance of @racket[parsed-package-query].

A @deftech{well-formed package query} is a @tech{parsed package query}
that uses strings for all fields.

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
instance of @racket[parsed-package-query] using @racket[(apply
parsed-package-query (string-split Q ":"))].

In the general case where fields may be omitted, any missing fields
should be set to the empty string like so:

@racketblock[
(define (parse-package-query s)
  (define user-defined (string-split s ":"))
  (define num-fields (length user-defined))
  (apply parsed-package-query
         (build-list (procedure-arity parsed-package-query)
                     (λ (i)
                       (if (< i num-fields)
                           (list-ref user-defined i)
                           "")))))
]


@section{Package Query API}

@defstruct*[parsed-package-query ([provider-name string?]
                                  [package-name string?]
                                  [edition-name string?]
                                  [revision-min string?]
                                  [revision-max string?]
                                  [interval-bounds string?])]{
A parsed form of a @tech{package query}.
}

@deftogether[(
@defproc[(well-formed-package-query? [v any/c]) boolean?]
@defproc[(malformed-package-query? [v any/c]) boolean?]
@defproc[(resolved-package-query? [v any/c]) boolean?]
@defproc[(exact-package-query? [v any/c]) boolean?]
)]{
Each procedure returns @racket[#t] if @racket[v] matches the
respective rule in @secref{pkg-query-classifications}.
}


@defthing[package-query? predicate/c]{
Returns @racket[#t] if the input value is suitable for use as an
argument to @racket[parse-package-query].
}


@defthing[package-query-variant? predicate/c]{
Returns @racket[#t] if the input value is suitable for use as an
argument to @racket[coerce-parsed-package-query]. Specifically, the
input can be a string, or an instance of
@racket[parsed-package-query].
}

@defthing[boundary-flags-string? predicate/c]{
Returns @racket[#t] if the argument is a suitable value for
@racket[package-query-interval-bounds].
}


@defproc[(coerce-parsed-package-query [variant package-query-variant?]) parsed-package-query?]{
Returns a @racket[parsed-package-query] from a variant value type.
}

@defproc[(format-parsed-package-query [query well-formed-package-query?]) string?]{
Converts a @racket[parsed-package-query] to a string, with no validation
performed on the fields in advance. The fields set in @racket[query]
are joined into the string as-is.
}

@defproc[(make-exact-package-query [provider string?] [name string?] [revision-number revision-number?]) exact-package-query?]{
Returns an @tech{exact package query} build from the arguments.
}

@defproc[(resolve-revision-interval [query parsed-package-query?]
                                    [make-revision-number
                                     (-> boolean? string? revision-number?)]
                                    [#:default-bounds default-bounds "ii"])
         (values revision-number? revision-number?)]{
Returns values created from @racket[make-revision-interval].  The
arguments for @racket[make-revision-interval] is based on the
information in @racket[query], such that each revision endpoint is
first transformed to a @tech{revision number} using
@racket[make-revision-number].

@racket[make-revision-number] accepts two arguments.

If the first argument is @racket[#f], then the second argument is
the same value bound to @racket[(parsed-package-query-revision-min query)].

If the first argument is @racket[#t], then the second argument is
the same value bound to @racket[(parsed-package-query-revision-max query)].

The first argument is useful for generating mock data for tests, but
is otherwise unhelpful when normalizing an arbitrary @tech{revision}
to a @tech{revision number}.

The output integers are adjusted according to
@racket[(parsed-package-query-interval-bounds query)], or
@racket[default-bounds] if @racket[(boundary-flags-string?
(parsed-package-query-interval-bounds query))] is @racket[#f].
}

@defproc[(abbreviate-exact-package-query [query exact-package-query?]) string?]{
Like @racket[format-parsed-package-query], except the resulting string omits
more fields. Since @racket[query] is exact, this does not result in
lost information.
}

@defproc[(parse-package-query [str string?]) parsed-package-query?]{
Converns a string to an instance of @racket[parsed-package-query].
}
