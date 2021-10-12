#lang scribble/manual

@require[@for-label[racket
                    denxi/query
                    denxi/string
                    denxi/version]
          "../../shared.rkt"]

@title{Package Queries}

@defmodule[denxi/query]

A @deftech{package query} is a colon-separated string used to find
@tech{package definitions}.


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
instead. See @secref{ac-iface} for more information.

The empty string is equivalent to using only default values.


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



@section[#:tag "queries"]{Examples}

Field positions are important. You can still create a query for just
@tt{calculator}, but @tt{calculator} would be intepreted as the
provider, and not the package name.


@subsection{Specifying an Edition}

If you prefer a scientific calculator, the package author can provide
that design under a different @tech/denxi-reference{edition}. Specify
an edition using the next field.

@verbatim|{
example.com:calculator:scientific
}|


@subsection{Specifying Accepted Revisions}

The next field is for requesting a specific
@tech/denxi-reference{revision} of a package.

@verbatim|{
example.com:calculator:scientific:288
example.com:calculator:scientific:open-beta
}|

A revision can be an exact nonnegative integer or a name. Names are
aliases for numbers.

What about version ranges? Just add another revision to act as the
maximum accepted revision.

@verbatim|{
example.com:calculator:scientific:288:288
}|

From here we can change the endpoints of the interval to accept alternative
packages.  This is useful if some implementations are not available.

@verbatim|{
example.com:calculator:scientific:102:288
}|


@subsection{Marking Inclusive and Exclusive Endpoints}

By default, revision intervals are inclusive of their endpoints.  You
can add flags to mark the interval as inclusive or exclusive of each
endpoint. Use the letter @tt{i} for inclusive, and @tt{e} for
exclusive.  In the below form, revision @tt{288} will @italic{not}
match this query because of the @tt{e} on the right side of the two
flags.

@verbatim|{
example.com:calculator:scientific:102:288:ie
}|

Using integer interval notation:

@itemlist[
@item{@tt{ii} means @litchar|{{102 .. 288}}|}
@item{@tt{ie} means @litchar|{{102 .. 287}}|}
@item{@tt{ei} means @litchar|{{103 .. 288}}|}
@item{@tt{ee} means @litchar|{{103 .. 287}}|}
]

Marking exclusive bounds are useful with revision names.  The below
query requests a scientific calculator's closed beta implementation,
up to but not including the production-ready revision.

@verbatim|{
example.com:calculator:scientific:closed-beta:production:ie
}|


If the author did not define a revision name marking the end of a
beta, then you would have to know the revision number in advance of
writing the query. With the interval flags, you do not have to know
any revision numbers.

When resolving @tech/denxi-reference{revision names}, Denxi will raise
an error for queries like these because they each resolve to a
backwards interval:

@verbatim|{
example.com:calculator:scientific:production:closed-beta
example.com:calculator:scientific:9:0
example.com:calculator:scientific:3:3:ee
}|


@subsection{Omitting Information}

You may omit fields in @tech{package queries}. Two contiguous colons
will set the associated field to the empty string. Any contiguous
colon sequence at the end of a query is implied and does not need to
be typed.

@verbatim|{
example.com:calculator::production
}|

Even the empty string is a valid @tech{package query}.  In fact,
@racket[""] and @racket[":::"] parse the same way.


@section{Primitive Package Query Operations}

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
Returns an @tech{exact package query} built from the arguments.
}

@defproc[(resolve-minimum-revision [query parsed-package-query?]
                                   [find-revision-number (-> string? any/c)])
                                   any/c]{
Returns @racket[(parsed-package-query-revision-min query)] as
a @tech{revision number}, taking boundary flags into account.

Pass the implementation for translating @tech{revision names} to
@tech{revision numbers} as @racket[find-revision-number].  When
necessary, @racket[resolve-minimum-revision] will apply
@racket[find-revision-number] to a @tech{revision name} to
resolve.

@racket[find-revision-number] may return something other than a
revision number. @racket[resolve-minimum-revision] will return that
value as-is. @racket[find-revision-number] may also raise an error,
which will be caught and returned only if it's not an instance of
@racket[exn:break]. Leverage these rules to understand why a
particular resolution failed (e.g. a web service 404s for a minimum
revision name, but not the maximum).

If @racket[find-revision-number] returns a revision number, it is
adjusted according to @racket[(parsed-package-query-interval-bounds
query)] using @racket[make-minimum-revision-number].
}


@defproc[(resolve-maximum-revision [query parsed-package-query?]
                                   [find-revision-number (-> string? any/c)])
                                   any/c]{
Like @racket[resolve-minimum-revision], but for
@racket[(parsed-package-query-revision-max query)].
}

@defproc[(abbreviate-exact-package-query [query exact-package-query?]) string?]{
Like @racket[format-parsed-package-query], except the resulting string omits
more fields. Since @racket[query] is exact, this does not result in
lost information.
}


@defproc[(parse-package-query [str string?]) parsed-package-query?]{
Converts a string to an instance of @racket[parsed-package-query].
}


@section[#:tag "ac-iface"]{Autocompleting Package Queries}

This section covers autocompleting package queries, which entails
replacing empty strings and non-string elements of
@racket[parsed-package-query] instances with well-formed values.

@defidform[gen:package-query-defaults]{
A @tech/reference{generic interface} for a structure type that
predicts missing fields for @tech{package queries}. The available
method definitions are documented in this section.
}

@defthing[package-query-defaults? predicate/c]{
Returns @racket[#t] if the sole argument is an instance of a structure
that implements at least zero methods of
@racket[gen:package-query-defaults].
}

@defthing[package-query-defaults-implementation/c chaperone-contract?]{
A contract for structure instances that implement at least zero
methods of @racket[gen:package-query-defaults]. The contracts for
existing methods match their documented contracts.  Such a structure
can autocomplete @tech{parsed package queries} using
@racket[autocomplete-parsed-package-query].
}

@defthing[default-package-query-defaults package-query-defaults-implementation/c]{
A default implementation for @racket[gen:package-query-defaults].

It implements every method using the same code as the fallbacks for
the generic interface.
}

@defproc[(autocomplete-parsed-package-query [autocomplete package-query-defaults-implementation/c]
                                            [query parsed-package-query?])
                                            well-formed-package-query?]{
Replaces all empty strings and non-strings in @racket[query] with
suitable values. @racket[autocomplete] may opt to preserve empty
strings.
}

@defproc[(get-default-provider [defaults package-query-defaults?]) string?]{
A method of @racket[gen:package-query-defaults] that returns a default
value for @racket[parsed-package-query-provider-name].

The fallback implementation for this method always returns @racket[DEFAULT_STRING].
}

@defproc[(get-default-package [defaults package-query-defaults?] [provider string?]) string?]{
A method of @racket[gen:package-query-defaults] that returns a default
value for @racket[parsed-package-query-package-name], in terms of a
provider name.

The fallback implementation for this method always returns @racket[DEFAULT_STRING].
}

@defproc[(get-default-edition [defaults package-query-defaults?]
                              [provider string?]
                              [package-name string?]) string?]{
A method of @racket[gen:package-query-defaults] that returns a default
value for @racket[parsed-package-query-edition-name], in terms of a
provider name and package name.

The fallback implementation for this method always returns @racket[DEFAULT_STRING].
}

@defproc[(get-default-min-revision [defaults package-query-defaults?]
                                   [provider string?]
                                   [package-name string?]
                                   [edition string?]) string?]{
A method of @racket[gen:package-query-defaults] that returns a default
value for @racket[parsed-package-query-revision-min], in terms of a
provider name, package name, and edition name.

The fallback implementation for this method always returns @racket{0}.
}

@defproc[(get-default-max-revision [defaults package-query-defaults?]
                                   [provider string?]
                                   [package-name string?]
                                   [edition string?]
                                   [min-revision string?]) string?]{
A method of @racket[gen:package-query-defaults] that returns a default
value for @racket[parsed-package-query-revision-max], in terms of a
provider name, package name, and edition name.

The fallback implementation for this method always returns @racket[DEFAULT_STRING].
}

@defproc[(get-default-interval-bounds [c package-query-defaults?]) string?]{
A method of @racket[gen:package-query-defaults] that returns a default
value for @racket[parsed-package-query-interval-bounds].

The fallback implementation for this method always returns @racket{ii}.
}


@section[#:tag "cq-iface"]{Creating Canonical Package Queries}

@tech{Package queries} may or may not refer to existing data at some
location. They require an authority to state that not only is a
package query meaningful, but the authority can direct a program to
the data using an exact variant of the query that it will provide.

A @deftech{package query canon} (or just “canon” in the context of
this section) is an implementation of @racket[gen:package-query-canon]
that represents the authority. A package query produced using
@racket[make-canonical-package-query] is said to be in
@deftech{canonical form} with respect to a canon.  We can also call
the result a @deftech{canonical package query}.

A @tech{canonical package query} is an @tech{exact package query}.
There is no predicate for canonical package queries, so the only
meaningful difference is that canonical package queries are expected
to work with the canon that defines them.

Therefore, @racket[make-canonical-package-query] represents a
@italic{social} contract for a @tech{package query canon} to only make
valid queries. If it is not able to do this, it should not return a
query at all.


@defidform[gen:package-query-canon]{
A @tech/reference{generic interface} for a structure type that creates
@tech{canonical package queries}. The available method definitions are
documented in this section.
}

@defthing[package-query-canon? predicate/c]{
Returns @racket[#t] if the sole argument is an instance of a structure
that implements at least zero methods of
@racket[gen:package-query-canon].
}

@defthing[package-query-canon-implementation/c chaperone-contract?]{
A contract for structure instances that implement all methods of
@racket[gen:package-query-canon]. The contracts for existing methods
match their documented contracts.  Such a structure is suitable for use
with @racket[make-canonical-package-query].
}


@defproc[(make-canonical-package-query
         [#:force-complete-interval? force-complete-interval? any/c #f]
         [canon package-query-canon-implementation/c]
         [defaults package-query-defaults-implementation/c]
         [query package-query-variant?])
         (subprogram/c exact-package-query?)]{
Returns a @tech{subprogram} that computes an @tech{exact package
query} in @tech{canonical form} with respect to @racket[canon]. In the
event of an error, the @tech{subprogram log} gains at least one
@racket[$package-query-canon] instance.

@itemlist[#:style 'ordered
@item{Autocomplete @racket[query] using @racket[(autocomplete-parsed-package-query defaults query)].}
@item{Convert all @tech{revision names} in the output of Step 1 to a @tech{revision number} interval.}
@item{Select exactly one revision number from the interval defined in Step 2 that points to an available package definition.}
@item{Return an @tech{exact package query} using the output of Step 3.}
]

In the event only one of the two revision interval endpoints is
resolved, the return value depends on
@racket[force-complete-interval?]. If
@racket[force-complete-interval?] is a true value, the process will
skip Step 3 and use the only available revision number to perform Step
4. Be careful with this option, because it discards the error
information for the unresolved boundary.
}


@defstruct*[($package-query-canon $message)
            ([user-query package-query?]
             [autocompleted-query package-query?]) #:prefab]{
A @tech{message} about a failure to convert a @tech{package query} to
@tech{canonical form}.

@racket[user-query] reflects what was passed to
@racket[make-canonical-package-query]. @racket[autocompleted-query] is
the result of applying @racket[autocomplete-parsed-package-query] to
@racket[user-query] in advance of the attempted conversion.
}

@defstruct*[($package-query-canon:backwards $package-query-canon)
            ([alleged-minimum revision-number?]
             [alleged-maximum revision-number?])]{
A @tech{package query canon} successfully converted @tech{revision
names} in a query to @tech{revision numbers}, but the numbers form the
backwards interval @litchar|{{alleged-minimum .. alleged-maximum}}|.
}

@defstruct*[($package-query-canon:no-minimum $package-query-canon) ([hint any/c])]{
A @tech{package query canon} could not resolve the @tech{revision
name} for the minimum revision of a package query.

@racket[hint] is the value—or a derived serializeable value—created
by applying @racket[resolve-minimum-revision] to the autocompleted
query. It may clarify why the revision name did not resolve.
}

@defstruct*[($package-query-canon:no-maximum $package-query-canon) ([hint any/c])]{
Like @racket[$package-query-canon:no-minimum] for a maximum @tech{revision name}.
}

@defstruct*[($package-query-canon:no-selection $package-query-canon)
            ([minimum revision-number?]
             [maximum revision-number?]
             [hint any/c])]{
A @tech{package query canon} resolved all @tech{revision names} to
@tech{revision numbers}, but could not select the best fit among them.

@racket[hint] is an implementation-specific value meant to explain why
this is the case.
}

@defstruct*[($package-query-canon:oob $package-query-canon)
            ([minimum revision-number?]
             [maximum revision-number?]
             [selection revision-number?])]{
A @tech{package query canon} resolved all @tech{revision names} to
@tech{revision numbers}, but selected a revision number outside
of the user's @litchar|{{minimum ... maximum}}| interval.
}


@defproc[(find-revision-number [canon package-query-canon-implementation/c]
                               [provider string?]
                               [package string?]
                               [edition string?]
                               [revision-name string?])
                               any/c]{
A method of @racket[gen:package-query-canon] that returns either the
@tech{revision number} named by @racket[revision-name], or an
implementation-defined value representing why a revision number cannot
be returned.
}

@defproc[(select-revision-number [canon package-query-canon-implementation/c]
                                 [provider string?]
                                 [package string?]
                                 [edition string?]
                                 [revision-min revision-number?]
                                 [revision-max revision-number?])
                                 any/c]{
A method of @racket[gen:package-query-canon] used by
@racket[make-canonical-package-query], after
@racket[find-revision-number].  It returns either a @tech{revision
number} or an implementation-defined value representing why a revision
number cannot be returned.

@racket[revision-min] and @racket[revision-max] are boundaries to an
inclusive integer interval. The @tech{revisions} they represent may
not exist where definitions would be stored.  If
@racket[select-revision-number] returns a @tech{revision number}, it
will be constrained to @litchar|{{revision-min ... revision-max}}| to
represent the best available revision (Typically this is the revision
number closest to @racket[revision-max]).

@bold{Warning:} Only return a revision number from your implementation
if the caller may assume that the package definition it references
actually exists. Failure to do so will cause a runtime error.
}
