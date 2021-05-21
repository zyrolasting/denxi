#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title[#:tag "queries"]{Queries}

A @deftech{package query} is a colon-separated string used to identify
@tech{package definitions}. An example looks like
@tt{example.com:calculator}, and that one is understood to mean “the
@tt{calculator} package provided by @tt{example.com}.”

Why not just say @tt{calculator}? Because providers are presumed
unique, and using a verifiable identity like a domain name helps
establish a name for your software across hosts. It also establishes a
basis for comparison when two developers use the same package name but
deliver different work.

Field positions are important. You can still create a query for just
@tt{calculator}, but @tt{calculator} would be intepreted as the
provider, and not the package name.


@section{Specifying an Edition}

If you prefer a scientific calculator, the package author can provide
that design under a different @tech{edition}. Specify an edition using
the next field.

@verbatim|{
example.com:calculator:scientific
}|


@section{Specifying Accepted Revisions}

The next field is for requesting a specific @tech{revision} of a package.

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


@section{Marking Inclusive and Exclusive Endpoints}

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

When resolving @tech{revision names}, Xiden will raise an error for
queries like these because they each resolve to a backwards interval:

@verbatim|{
example.com:calculator:scientific:production:closed-beta
example.com:calculator:scientific:9:0
example.com:calculator:scientific:3:3:ee
}|


@section{Omitting Information}

You may omit fields in @tech{package queries}. Two contiguous colons
will set the associated field to the empty string. Any contiguous
colon sequence at the end of a query is implied and does not need to
be typed.

@verbatim|{
example.com:calculator::production
}|

Even the empty string is a valid @tech{package query}.  In fact,
@racket[""] and @racket[":::"] parse the same way.
