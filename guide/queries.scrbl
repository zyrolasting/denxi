#lang scribble/manual

@require["shared.rkt" @for-label[racket/base]]

@title[#:tag "asking"]{Package Queries}

You can request packages using a colon-separated @tech{query} string.

@tt{john.doe:calculator} means "the @tt{calculator} package by
@tt{john.doe}". But it turns out that @tt{john.doe:calculator} is just an
abbreviation for @tt{john.doe:calculator:draft:newest}.  @tt{draft} is the
package's @tech{edition}. @tt{newest} is the edition's @tech{revision}.  Both
@tt{draft} and @tt{newest} are just default values.

So, @tt{john.doe:calculator:draft:newest} means "the latest revision of the
draft edition of the @tt{calculator} package by @tt{john.doe}."

If you prefer a scientific calculator, the package author can provide that
design as a diferent edition. In that case you replace @tt{draft} with
@tt{scientific}.

@verbatim|{
john.doe:calculator:scientific:newest
}|

To request an @italic{exact} version of a package, replace @tt{newest} with a
@tech{revision number} or another @tech{revision name}. The following examples
are @tech{exact queries} because they request specific implementations of
John's scientific calculator.

@verbatim|{
john.doe:calculator:scientific:288
john.doe:calculator:scientific:with-trig
}|

Revision names are aliases or revision numbers, but the numbers are necessary
to comparing versions and provide a standard form for revisions. @tt{newest} is
special for being the only @tech{revision name} that can refer to more than one
implementation. In other words, all queries that use @tt{newest} are
@tech{inexact queries}.

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

You can add flags to mark the interval as inclusive or exclusive of
each endpoint. Use the letter @tt{i} for inclusive, and @tt{e} for
exclusive.  In the below form, revision @tt{288} will @italic{not} be
included because of the @tt{e} right next to it.

@verbatim|{
john.doe:calculator:scientific:i:102:e:288
}|

There's a problem: Someone can't read this and know why this query makes sense
for your project. You can write a comment, but let's say John developed his
scientific calculator through an invite-only beta. John later put out a
production-ready copy in response to feedback, along with the beta revisions
for posterity. You can still use names in place of the numbers to express a
preference for revisions made during the closed beta.

@verbatim|{
john.doe:calculator:scientific:i:closed-beta:e:production
}|

When resolving @tech{revision names}, @binary will reject queries
like these because they each create an invalid interval:

@verbatim|{
john.doe:calculator:scientific:production:closed-beta
john.doe:calculator:scientific:9:0
john.doe:calculator:scientific:e:3:e:3
}|

Formally, a @tech{query} string follows this EBNF grammar:

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
