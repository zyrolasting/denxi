#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title[#:tag "versioning"]{Versioning Scheme}

Xiden versions software like books to help users discover software,
and to help programmers segment their users and code without negative
impact on a brand.


@section{Editions}

An @deftech{edition} is a name for a design or target audience. Think
of it as a semantic alternative to a major version number. When you
wish to adapt your software to a different audience without disrupting
existing users, change the edition.

Editions address technical and social concerns. They allow you to
divide up your user base without changing the (branded) name of your
software or the general value it offers. This may be preferable to
forcing one implementation to accomodate everyone. You can use this as
an excuse to refactor, rewrite, or delete swaths of code that are
irrelevant to an audience.  It also helps one avoid nonsensical
numerical schemes that try to track breaking changes across all
possible audiences.

Define editions sparingly and thoughtfully to keep maintenance costs
manageable.


@section{Revisions}

A @deftech{revision} is a numbered implementation of an @tech{edition}. Given
an edition, a user can select a @tech{package definition} using a
@tech{revision number} or a @tech{revision name}.


@subsection{Revision Numbers}

A @deftech{revision number} is an exact non-negative integer.

If you are releasing a new @tech{package definition} with the same
@tech{edition}, then increment the @tech{revision number}. If you are
starting a new edition, then the @tech{revision number} must start at
@racket[0] as it does in the examples on this page.


@subsection{Revision Names}

A @deftech{revision name} is an alias for a @tech{revision number}. A
@tech{revision name} can be any string that contains at least one non-digit.

A @tech{package definition} may include a list of at least zero @tech{revision
names} for the @tech{revision number}.

@bold{A @tech{revision name} should be unique within an
@tech{edition}}.  If a user searches for a package definition using a
@tech{revision name}, and that name refers to more than one
@tech{revision number} in an @tech{edition}, then the provider is
responsible for correcting the ambiguity.


@section{Optional Reading: What About Semantic Versioning?}

@hyperlink["https://semver.org/"]{Semantic Versioning} (“SemVer”)
allows you to infer what happened to software depending on which of
three version numbers changed. Any assumptions about the change hold
so long as the relevant package author follows SemVer rules closely
and voluntarily. Since SemVer depends on ideal human compliance like
every other scheme, I don't think it contributes much in the end.

In my mind, a version is a form of identification. Jeff Atwood used
the term “dog tag,” since we often ask a user what version they are
using when our program fails out in the field. This came before the
simple advice to just use a date or timestamp for each version.

This makes sense, but neither scheme helps users with discovery. No
one knows if version 3.49.111 or Oct-10-2017 is a good fit for them
without additional context. But even non-technical users would
understand that they probably want a specific revision of a teacher's
edition. I don't see why software should be any harder to evaluate
than a book. If I later extended this idea to a versioning scheme that
not only identified software, but captured the nature of all related
changes, then I @italic{might} tell you after consulting a patent
attorney.

We can do without the bureaucracy and social pressure of rules like
those defined in SemVer. The scheme defined on this page offers no
pretense that you can look at two versions and know if the most recent
one is backwards-compatible. All this scheme does is give you room to
divide up your userbase into audiences and your product into designs.
Then, you can chronicle releases in terms of @italic{your relationship
with each audience}. This can be messy, but that's life. At least with
this scheme you can decide what changes to make in terms of the people
affected by those changes. That's as good as it gets.

If you still prefer Semantic Versioning after reading all that, then
you can always define an @tech{edition} that uses semantic version
strings as @tech{revision names} and use a plugin that resolves
Semantic Version queries. See @secref["Plugins" #:doc '(lib
"xiden/docs/reference/xiden-reference.scrbl")] for more information.
