#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title[#:tag "versioning"]{Versioning Packages}

Every @tech{package definition} has an @tech{edition} and a
@tech{revision}.

@section{Editions}

An @deftech{edition} is a name for a design or target audience. Think
of it as a semantic alternative to a major version number. When you
wish to adapt your software to a different audience without disrupting
existing users, change the edition.

The default name of an edition is @racket{default}.

Editions address engineering and marketing concerns:

@itemlist[

@item{When marketing, editions allow you to segment your user base
without changing the (branded) name of your software or the general
value it offers. In some cases, this is preferable to forcing one
implementation to accomodate everyone.}

@item{When engineering, editions provide an excuse to refactor, rewrite, or
delete swaths of code that are irrelevant to an audience.  It also
helps one avoid nonsensical numerical schemes that try to track
breaking changes across all possible audiences.}

]

Be thoughtful with edition names. Define them sparingly with the
intent to commit to each name.  Because you know someone out there is
going to use names like @racket{teacher1}, @racket{students2020},
@racket{mrsbakermiddleschool}.


@section{Revisions}

A @deftech{revision} is a numbered implementation of an
@tech{edition}. Given an edition, a user can select a @tech{package
definition} using a @tech{revision number} or a @tech{revision
name}.

A @deftech{revision number} is a non-negative integer. A
@deftech{revision name} is an alias for a @tech{revision number}. A
@tech{revision name} can be any string that contains at least one
non-digit.

A @tech{package definition} must include both a @tech{revision number}
and a list of at least zero @tech{revision names} for that
number. When changed, a package must increment its @tech{revision
number} if the change uses the same @tech{edition}. If the package
starts a new edition, the @tech{revision number} must start at
@racket[0].

A @tech{revision name} should be unique within an @tech{edition}.  If
a user searches for a package definition using a @tech{revision name},
and that name refers to more than one @tech{revision number} in an
@tech{edition}, then this should be seen as a mistake on the
provider's part.


@section{Opinion: What About Semantic Versioning?}

@hyperlink["https://semver.org/"]{Semantic Versioning} (“SemVer”)
allows you to infer what happened to software depending on which of
three version numbers changed. Any assumptions about the change hold
so long as the relevant package author follows SemVer rules closely
and voluntarily. Since SemVer depends on ideal human compliance like
every other scheme, I don't trust it unconditionally.

In my mind, a version is a form of identification. Jeff Atwood used
the term “dog tag,” since we often ask about version once a program
fails in the field. This makes sense, but it also helps to use
versions to help people find what they want. Non-technical users don't
know that 3.49.111 is what they want, most of them @italic{do}
understand the cover of a book. Certainly well enough to discriminate
between two editions or revisions of the same title. I don't see why
software should be any different. If I later extended this idea to a
versioning scheme that not only identified software, but captured the
nature of all related changes, then I would only tell you long after
consulting a patent attorney.

We can do without the bureaucracy and social pressure of rules like
those defined in SemVer. The scheme defined on this page offers no
pretense that you can look at two versions and know if the most recent
one is backwards-compatible. All this scheme does is give you room to
divide up your userbase into audiences and your product into designs.
Then, you can chronicle releases in terms of your relationship with
each audience. This is messy, but that's life. At least with this
scheme you can decide what changes to make in terms of the people
affected by those changes. That's as good as it gets.

If you still prefer Semantic Versioning after reading all that, then
you can always define an @tech{edition} that uses semantic version
strings as @tech{revision names} and use a plugin that resolves
Semantic Version queries. See @secref{plugin} for more information.
