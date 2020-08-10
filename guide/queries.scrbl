#lang scribble/manual

@require["shared.rkt" @for-label[racket/base]]

@title[#:tag "asking"]{Package Queries}

You can request packages using a colon-separated @tech{query} string.

@tt{john.doe:calculator} means "the @tt{calculator} package by
@tt{john.doe}". But it turns out that @tt{john.doe:calculator} is just an
abbreviation for @tt{john.doe:calculator:draft:newest}.  @tt{draft} is the
package's @tech{edition}. @tt{newest} is the edition's @tech{revision}.  Both
@tt{draft} and @tt{newest} are just default values.

So, @tt{john.doe:calculator:draft:newest} means "the @tt{newest} revision of the
@tt{draft} edition of the @tt{calculator} package by @tt{john.doe}."


@section{Specifying an Edition}

If you prefer a scientific calculator, the package author can provide
that design as a diferent @tech{edition}. If John Doe releases a
@tt{scientific} edition for the @tt{calculator} package, then you can
replace @tt{draft} with @tt{scientific}.

@verbatim|{
john.doe:calculator:scientific:newest
}|


@section{Specifying a Revision}

To request a specific @tech{revision} of a package, replace
@tt{newest} with a @tech{revision number} or another @tech{revision
name}.

@verbatim|{
john.doe:calculator:scientific:288
john.doe:calculator:scientific:with-trig
}|

Revision names are aliases or revision numbers, but the numbers are necessary
to comparing versions and provide a standard form for revisions. @tt{newest} is
special for being the only @tech{revision name} that can refer to more than one
implementation. In other words, all queries that use @tt{newest} are
@tech{inexact queries}.


@section{Specifying a Version Range}

What about version ranges? When you ask for
@tt{john.doe:calculator:scientific:288}, you are actually asking for the latest
package in an inclusive interval that just happens to contain only one
package. You can rewrite the query to make this interval explicit.

@verbatim|{
john.doe:calculator:scientific:288:288
}|

From here we can change the endpoints of the interval to accept alternative
packages.  This is useful if some implementations are not available.

@verbatim|{
john.doe:calculator:scientific:102:288
}|


@section{Marking Inclusive and Exclusive Endpoints}

You can add flags to mark the interval as inclusive or exclusive of
each endpoint. Use the letter @tt{i} for inclusive, and @tt{e} for
exclusive.  In the below form, revision @tt{288} will @italic{not} be
included because of the @tt{e} right next to it.

@verbatim|{
john.doe:calculator:scientific:i:102:e:288
}|


@section{Revision Names Create Human-Readable Intervals}

There's a problem: Someone can't read this and know why this query makes sense
for your project. You can write a comment, but let's say John developed his
scientific calculator through an invite-only beta. John later put out a
production-ready copy in response to feedback, along with the beta revisions
for posterity. You can still use names in place of the numbers to express a
preference for revisions made during the closed beta.

@verbatim|{
john.doe:calculator:scientific:i:closed-beta:e:production
}|


@section{Catching Reversed Intervals}

When resolving @tech{revision names}, @binary will reject queries
like these because they each create an invalid interval:

@verbatim|{
john.doe:calculator:scientific:production:closed-beta
john.doe:calculator:scientific:9:0
john.doe:calculator:scientific:e:3:e:3
}|


@section{Exact Queries in an Inexact World}

Both @tt{john.doe:calculator:scientific:288} and
@tt{john.doe:calculator:scientific:with-trig} are @tech{exact queries}
in the context of a single server. But if @binary uses these queries
to collect information from more than one server, it's possible that
both servers will conflicting information.

The cause of this discrepency is human behavior, which no software can
correct. @secref{verification} covers how you can verify that a
package is exactly what you expect it to be on your system.

In practice, different answers for the same @tech{exact queries} can
be addressed on a case-by-case basis. For example, @tt{john.doe} as a
provider might guarentee that @tt{with-trig} will refer to the same
revision supporting trigonometric functions, even if the revision
numbers differ.


@section{Query Grammar}

A @tech{query} string follows this EBNF grammar:

@verbatim|{
<package-query> ::= <package-identity> | <package-identity> ":" <version>

<package-identity> ::= <name> ":" <name>

<version> ::= <name> | <name> ":" <revision>

<revision> ::= <revision-boundary> | <inclusive-revision-range> | <general-revision-range>

<revision-boundary> ::= <name> | <revision-number>

<revision-number> ::= <non-zero-digit> <digit>*

<inclusive-revision-range> ::= <revision-boundary> ":" <revision-boundary>

<general-revision-range> ::= <interval-flag> ":" <revision-boundary> ":" <interval-flag> ":" <revision-boundary>

<interval-flag> ::= "e" | "i"

<non-zero-digit> ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

<name> ::= ? A string representing a legal file name ?
}|

@binary uses @tt{<name>}s for directory and link names on file systems. For
that reason, a package named @tt{aux} cannot appear on a Windows system.
@binary will alert you if a package uses a reserved file name.
