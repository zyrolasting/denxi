#lang scribble/manual

@require[@for-label[racket/base
@except-in[denxi/launcher #%module-begin]]
         "../shared.rkt"]

@title[#:style '(toc)]{Denxi White Paper}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

@define[sh-link "https://github.com/zyrolasting/denxi/tree/master/examples/04-self-hosting"]

This paper covers software distribution problems, and how Denxi
handles them.

For all documentation, see @other-doc[denxi-index].


@section{Why You Should Care About Software Distribution}

@(define tesla-link "https://teslamotorsclub.com/tmc/threads/car-wont-start-after-software-update.220433/")

Software distribution is a nasty problem domain that affects every
user regardless of technical skill. Even laypersons will find
themselves unable to @hyperlink[tesla-link]{start their cars} after a
bad update.

@secref{scenarios} shows that there are so many ways software
distribution can go wrong, so you need a way to download the latest
software while isolating the parts that don't work for you.  When you
aren't sure how to do that, you still want to know there's a way to
have an independent developer @italic{you} trust fix @italic{your}
package mananger in the same way an independent mechanic would fix
your bricked car.

Denxi is a free, open source, and self-hosting model. This means it
allows easy creation of its own alternatives (See
@secref{competition}). This empowers users and/or trusted developers
to handle bad updates, diamond dependencies, security incidents, and
other forms of human error without depending on the same middlemen.


@section{Intended Experience}

When you create a symbolic link--or a shortcut on Windows--you
normally point the link to a target location that already has a file,
directory, or link. What if you could express the link's target
subjectively?

It's easier to see what I mean using hypothetical commands.  Here are
some commands to create symbolic links.

@verbatim|{
ln -s ~/scanner scanner
ln -s ~/scribble scribble
ln -s ~/watch watch
}|

All of the above targets must exist if we want the links to work, but
the targets could contain anything. The point of dependency management
is to correctly reproduce the targets you @italic{meant}. Denxi lets
you express exact targets in your own (possibly inexact) words.  That
works something like this.

@verbatim|{
ln -s "$(semver scanner@8.4.x)" scanner
ln -s "$(github racket/scribble)" scribble
ln -s "$(urn watch:latest)" watch
}|

These snippets are simplistic illustrations. Denxi's actual interface
is more involved for reasons covered in @secref{secconv}, but you can
configure the default CLI to recognize custom notations.  The
experience of creating links remains the same, but Denxi does so in a
reproducible way despite generally ambiguous input.

This command shows a subset of Denxi's CLI, in the context of a custom
launcher named @litchar{my-denxi}. It may have the same semantics
you'd make up for the previous snippet.

@verbatim|{
./my-denxi do +d scanner scanner@8.4.x \
              +d github racket/scribble \
              +d watch watch:latest
}|

Installation, updates, and rollbacks are come down to making new
links. Reproducing dependencies means running the same
transaction. Once you finish using Denxi, you can write code as if
resolved dependencies are normal local files. To uninstall software,
delete the links you don't want and tell Denxi to delete everything
without any incoming links.

Denxi can manage dependencies for projects written in any language
that follows symbolic links when resolving modules or including files
in-place.


@section[#:tag "secconv"]{On Security and Convenience}

Denxi includes a zero-trust (meaning “Deny All by default”) launcher
called @litchar{denxi}. Assuming your Denxi instance is not
compromised, no installations are possible with @litchar{denxi}'s
default configuration. It takes effort to make it less safe to use,
and it can in fact become @italic{harder to use} as it evolves. You
could argue that it is user-hostile, but it represents
security-consciousness in its most extreme form for a high-level
language.

Denxi does make one exception in that has no concept of user accounts,
so it trusts whatever is directly controlling it. If you tell Denxi to
trust integrity checks that use MD5, then it will. You should restrict
the OS-level permissions of any process using Denxi, but the
zero-trust defaults make it harder to accidentally open
vulnerabilities.

The @racketmodname[denxi/launcher] DSL builds custom launchers that
represent the line between security and convenience. Custom launchers
are easier to use because they bake in all of the little annoying--yet
important!--decisions that a zero-trust launcher requires. You don't
have to expose Denxi's full interface for the 10% of functionality you
and your users need, but users can be confident that the 10% is
carefully considered.

You can create confidence and organize communities by sharing custom
launchers @italic{using} @litchar{denxi}. The custom launcher bakes in
surgical decisions and presents a convenient interface, and
@litchar{denxi} provides an unambiguous channel through which others
can audit how that launcher is distributed.

Since a Denxi launcher is just text, an end-user can edit it
themselves and repeat the whole process. Since every launcher can
represent a communual consensus or an individual preference, each user
can control their level of interaction with untrusted and
formerly-trusted people.


@section{Handling Dependency Hell}

Denxi detects circular dependencies and limits data duplication, even
in side-by-side installations of many different versions of the same
software. Denxi lets users and launchers choose their own reaction to
more difficult forms of dependency hell. For example, you can replace

@itemlist[
@item{all modules affected in a diamond dependency pattern}
@item{insecure payloads with patched equivalents}
@item{many different dependencies with one uniform dependency; and}
@item{any source code distribution with a pre-built binary}
]


@section[#:tag "competition"]{Following Racket's Pro-Competitive Example}

Denxi is written in Racket. Racket is a programming language, and
Denxi is a software distribution tool. Both are able to adapt in
interesting ways because they are what they easily create. Racket can
make you your own programming language, and Denxi can make you your
own dependency manager.

Denxi does not anticipate your needs because that's a mistake.  If a
tool fails to anticipate you want, it can't read your mind and
reprogram itself. That's why I designed Denxi to be like Racket, in
the sense it helps you create and prototype its own alternatives.

I believe Racket's package managers failed to translate the Racket
experience to software distribution as a domain. PLaneT and
@litchar{raco pkg} made many assumptions about how people will work
with them, which forces the surrounding community to work according to
those assumptions. My personal motivation to make Denxi came from
attempting to reconcile my soaring expectations of Racket with the
invariants of its package management system. I cannot critique
Racket's package manangers on subjective grounds, but I also could not
stop using them without giving up access to most of Racket's
ecosystem. There has to be a middle ground where you can change how
you get dependencies without isolating yourself from any community's
content. This will involve separating the subjective parts of software
distribution from the objective parts.

Denxi is built on the assumption that tools like it are going to keep
proliferating, and that we should have more that are completely
beholden to what a user expects from it. Denxi is free software, so
that when Denxi launchers proliferate on your system, you have say
over how they @italic{all} work, even in the strange and wonderful
situations I itemized in @secref{scenarios}.

Supporting experimentation and competition in this way is good for end
users, and as @secref{secconv} explains, it's a victory for informed
consent.


@section{Localization}

You can't share work effectively without a way to cross language
barriers. To aid translation, Denxi's output is a @racket[read]able
list of @tech/denxi-reference{messages} that one can think of as a
human language-independent document. @racketmodname[denxi/l10n]
translates these documents to reports in a specific human language. If
you store unlocalized messages in a file, @litchar{denxi show log}
will present the file in the user's chosen language.

At the time of this writing, Denxi only includes English as it's used
in the United States. However, a custom launcher may use your own
translations via @racket[format-message].


@section{Versioning}

Denxi versions software using @tech/denxi-reference{editions} and
@tech/denxi-reference{revisions}. Each edition has its own number line
for revisions. Revisions may have names that each map to exactly one
number on an edition's number line. An edition represents work for a
target audience, and revisions model change with respect to that
audience.

Versions are subjective, so you can override how they are interpreted
when prudent. If a set of versions identify software known to be
insecure, the you can dynamically replace the software used by one
version with an acceptable variant. If two names conflict, you can
decide which is canonical. See @secref{names} for more information.

The scheme allows a developer to rewrite code for a new audience
without affecting brand expectations. Users benefit because versions
contain discovery information, making it easier to decide what content
is relevant for them.


@section[#:tag "names"]{Handling Names}

Names are subjective, so Denxi recognizes no central authority on what
they mean. Any launcher can decide what names are canonical, and what
canonical names mean in a context-sensitive way.

For example, the SHA-1 cryptographic hash function has many
implementations and aliases. One may refer to SHA-1 using strings like
@racket{sha1}, @racket{SHA-1}, @racket{SHA1}, or
@racket{SHA_1}. Additionally, not all implementatons are created equal
in the eyes of an InfoSec expert. You can tell Denxi to see exactly
one of the aforementioned names as canonical, and bind that name to an
implementation you trust. The rest may be defined as aliases for the
canonical name.

The same applies to the names of parties and packages. If you
encounter two packages called @litchar{uri}, you may designate one as
canonical or install them side-by-side. This is powerful because it
means naming conflicts only happen when a user allows them to happen.

For another example, if you find a CSS package called @litchar{css}
that is incomplete and unmaintained, and another CSS package called
@litchar{better-css} that is more deserving of the plain @litchar{css}
name, you can assign the shorter name to the better implementation.


@section{Conclusion}

In this paper I explained why you should worry about software
distribution, and how Denxi gives you options when you inevitably want
to control updates in a more nuanced way.

I also explained that Denxi can be used to build and distribute its
own competition as free software.  Any user may form a community
around an individual launcher, or fork a community's launcher without
sacrificing the content it fetches. This model preserves free
association between people.

If you are interested in trying that for yourself, then read
@other-doc[denxi-guide].


@section[#:tag "scenarios"]{Addendum: Scenarios}

I've encountered scenarios that have second-, third-, and fourth-
order effects on my teams and clients. If you don't know what some of
these items mean, that's fine. This section indirectly summarizes
problems Denxi can solve, and my background.

@itemlist[

@item{A DIBOL project in a Subversion repository has one branch per
customer. When one customer gets a new feature, some other customers
want the same feature. None of their branches can merge automatically.}

@item{An update script that pulls unverified archives from an
executive's personal DropBox. It extracts the archive, clobbering
itself and other files in the running project.  The code controlled
large machines that could hurt somebody in over 40 locations in the
continental United States. No version control was available, so each
implementation is manually tweaked by on-site operators when the
DropBox deployment breaks.}

@item{A system that may or may not automatically reinstall a specific
application that day. If it does reinstall, we cannot predict what
version it would be.}

@item{A pile of Bash scripts that each expect files in magic
locations. The proper order of scripts is not clear, and calling the
wrong one means contractors can't submit work that day.}

@item{A marketing team that controls how Canadians and Australians
experience the corporate website, but not Puerto Ricans.}

@item{Programmers servicing unsupervised systems over TeamViewer.}

@item{Managers knowingly undoing a patch by moving files from their
“backup” thumb drive to hit some numbers for the day.}

@item{A catalog that won't store versioned artifacts and won't
guarentee availability for the one it happens to have.}

@item{Name conflicts between unrelated projects on account of their
directory structure.}

@item{A content marketplace that a child can use to hijack
presentations around the world.}

@item{Warehouse crews that @italic{won't} replace their Windows phones.}

@item{Programs that do not say they depend on each other, but won't
work unless you load them in a certain order.}

]
