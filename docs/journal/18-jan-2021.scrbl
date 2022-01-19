#lang scribble/report

@require["../shared.rkt" scribble/manual
         @for-label[racket/base racket/exn]]
@title{18 January 2021}
@by-slg

I'm still refactoring Denxi to allow user-defined discovery
information.

@section{Yak Shaving with Racket Mode}

@racket[denxi/subprogram] tests were failing only in
@litchar{racket-mode} 20220109.1535 and (I expect) many prior
releases. They set @racket[error-display-handler] to a custom value
when creating a REPL. Since @litchar{racket-mode} controls my code, I
could not simply capture the original handler in a module
instantiation.  The result: @racket[exn->string] behaves differently,
as it did in my tests.

I needed to introduce a predictable way to coerce values into strings
for use in machine-readable documents.  @racket[format-value] and
@racket[format-values] now exist in my copy of @racket[denxi/format]
for that purpose.

Moral of the story: Don't use @racket[exn->string] without an
implementation of @racket[error-display-handler] you firmly control.
If a function implementation uses @tech/reference{parameters} that a
caller cannot override with an argument, then the caller has to
remember to use @racket[parameterize] in addition to specifying
arguments.  A better name for @racket[exn->string] is
@racket[format-exn-in-parameterization].

I use parameters extensively because all functional behavior shares a
common, dynamic configuration. My experience with @racket[exn->string]
shows how Racket code is sensitive to its environment. I don't want
this sensitivity in Denxi.


@section{Abstract State}

Removing the fixed taxa meant destroying how Denxi's state works.
Denxi uses SQLite3, and the taxa are coupled to the database schema.
No fixed taxa, no database. No database, no queries. That leaves files
with missing context, and a pile of work.

I'd like to make all state interactions go through a cleaner interface
instead. That way Denxi's operations could be directed anywhere.

The core state operations are to safely transfer variable-length data
to an allocated location, and bind names to the location.

The bindings are not reliable, because allocated data might not be
available.

The names are user-defined, creating a need for canonicalization when
Denxi users wish to share work. I drafted a @racket[denxi/canon]
module that acts like a binary evaluator. The same value can be simply
cycled across domains of discourse according to user-defined
rules. The canonical value is what pops out. e.g. @racket{360} maps to
@racket[1], if we wanted a zero-based release number to identify
string Xbox releases. This also helps bring order to tagging systems,
where variations of the same tag are aliases of an official tag.

Denxi works by binding a canonical name to available data, then bind
additional references to aliases. But instead of doing that with only
a database and file system, it will do so in the abstract.  The
SQLite3+file approach will become one implementation of a
@tech/reference{generic inteface}.
