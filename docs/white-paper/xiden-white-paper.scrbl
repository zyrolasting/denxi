#lang scribble/manual

@require[@for-label[racket/base
@except-in[xiden/launcher #%module-begin]]
         "../shared.rkt"]

@title[#:style '(toc)]{Xiden White Paper}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

@define[sh-link "https://github.com/zyrolasting/xiden/tree/master/examples/04-self-hosting"]

This paper covers software distribution problems, and how Xiden
handles them.

For all documentation, see @other-doc[xiden-index].


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

Xiden is a serviceable, fully-exposed, and self-hosting model that
allows easy creation of its own alternatives (See
@secref{competition}). This empowers users and/or trusted developers
to handle bad updates, diamond dependencies, security incidents, and
other forms of human error without depending on middlemen.


@section{Intended Experience}

When you create a symbolic link (or a shortcut on Windows), you
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
the point of dependency management is reproducing the targets you
@italic{meant}. Xiden lets you express concrete targets in your own
words, which behaves something like this.

@verbatim|{
ln -s "$(semver scanner@8.4.x)" scanner
ln -s "$(github racket/scribble)" scribble
ln -s "$(urn watch:latest)" watch
}|

Xiden's actual interface is more involved for reasons covered in
@other-doc[xiden-guide], but you can extend it to recognize custom
notations and install dependencies in a single transaction.  The
end-user experience of creating links remains the same, but Xiden does
so in a reproducible way.

This command shows a subset of Xiden's CLI, in the context of a custom
launcher named @litchar{my-xiden}. It has the same meaning you'd make
up for the previous snippet.

@verbatim|{
./my-xiden do +d scanner scanner@8.4.x \
              +d github racket/scribble \
              +d watch watch:latest
}|

Installation, updates, and rollbacks are come down to making new
links. Reproducing dependencies means running the same
transaction. Once you finish using Xiden, you can write code as if
resolved dependencies are normal local files. To uninstall software,
delete the links you don't want and tell Xiden to delete everything
without any incoming links.

Xiden can manage dependencies for projects written in any language
that follows symbolic links when resolving modules or including files
in-place.


@section[#:tag "secconv"]{On Security and Convenience}

Xiden includes a zero-trust (think “Deny All”) launcher called
@litchar{xiden}. Assuming your Xiden instance is not compromised, no
installations are possible with @litchar{xiden}'s default
configuration. It takes effort to make it less safe to use, and it can
in fact become @italic{harder to use} as it evolves. You could argue
that it is user-hostile in its relentlessly bureaucratic nature. It
represents security-consciousness in its most extreme form for a
high-level language.

Xiden does make one exception: It trusts whatever is directly
controlling it. By that I mean if you tell Xiden to trust integrity
checks using MD5 digests, then it will. You should restrict the
OS-level permissions of any process using Xiden, but the zero-trust
defaults make it harder to accidentally open vulnerabilities.

The @racketmodname[xiden/launcher] DSL builds custom launchers that
represent the line between security and convenience. Custom launchers
are easier to use because they bake in all of the little annoying--yet
important!--decisions that a zero-trust launcher requires. You don't
have to expose Xiden's full interface for the 10% of functionality you
and your users need, but users can be confident that the 10% is
carefully considered.

You can create confidence and organize communities by sharing custom
launchers @italic{using} @litchar{xiden}. The custom launcher bakes in
surgical decisions and presents a convenient interface, and
@litchar{xiden} provides an unambiguous channel through which others
can audit how that launcher is distributed.

Since a Xiden launcher is just text, an end-user can edit it
themselves and repeat the whole process. Since every launcher can
represent a communual consensus or an individual preference, each user
can control their level of interaction with untrusted and
formerly-trusted people.


@section{On Dependency Hell}

Like Guix, Xiden uses functional programming principles. Any
dependency is viewed as an argument to a pure function that builds
dependent software.

This model helps Xiden detect circular dependencies and limit data
duplication, even in side-by-side installations of many different
versions of the same software.

Some examples of dependency hell have no easy answer. Conflicting
identities, diamond dependencies, availability of vulnerable software,
and generative bindings from functionally-equivalent programs can
thwart even the most robust systems. Most developers make judgement
calls to get past these problems, and then warn users not to do
certain things. Xiden lets users choose and change their own reaction
to these challenges, because as I said before, I want to preserve the
user's ability to choose their own tradeoffs.

For example, Xiden allows users to decide what names are canonical in
the event of a conflict. If two programmers claim the name
@litchar{john.doe} and try to distribute software under all the same
names, an end user may still distinguish the John Does' contributions
locally. If you get several apps that verify data integrity using
@racket['sha-256], @racket['Sha256], and @racket['SHA256], you can
tell Xiden which name is canonical and to treat the rest as aliases.

Most other forms of dependency hell are addressed using privileged
overriding features like @racket[current-package-editor]. Because an
entire software package is just an argument to a function, you can
swap it out or tinker with it before it makes any impact on your
system.

Overridding can replace

@itemlist[
@item{all modules affected in a diamond dependency pattern}
@item{insecure payloads with patched equivalents}
@item{many different dependencies with one uniform dependency; and}
@item{any source code distribution with a pre-built binary}
]

In brief, where Xiden cannot outright solve dependency hell, it is
designed to allow surgical responses such that a user is never
blocked. If you get tired of solving those problems over and over
again, just start from a launcher with your favored solutions.


@section[#:tag "competition"]{Following Racket's Pro-Competitive Example}

Xiden is written in Racket. Racket is a programming language, and
Xiden is a software distribution tool. Both are able to adapt in
interesting ways because they are what they easily create. Racket can
make you your own programming language, and Xiden make you your own
dependency manager.

Xiden is therefore not designed to anticipate human needs, because
that's a mistake. It's the kind of thinking that leads to tools that
expect humans to accomodate them. If a tool fails to anticipate what
users want, it can't read your mind and reprogram itself. I don't
think tools get opinions, and I don't think it's wise to assume that
Xiden will be the final answer on a domain this tricky. That's why I
designed Xiden to be like Racket in this space: To facilitate rapid
creation and prototyping of its own alternatives.

Oddly enough, I think Racket's own package managers failed to
translate the Racket experience to this space. PLaneT and
@litchar{raco pkg} made too many assumptions about how people will
work with them. My personal motivation to make Xiden came from
attempting to reconcile my soaring expectations of Racket with the
inflexibility of its package managers. I cannot critique Racket's
package manangers on subjective grounds, but I also could not easily
use something else without giving up a lot of features in Racket's
ecosystem. I feel like there has to be a middle ground here, and it
involves separating the subjective parts of software distribution from
the objective parts.

If you believe there are too many launchers like Steam, GOG, and the
App Store, then I understand. But Xiden is built on the assumption
that those platforms are going to keep proliferating, and that we
should have more that are completely beholden to what a user expects
from it. Xiden is free software, so that when Xiden launchers
proliferate on your system, you have say over how they @italic{all}
work, even in the strange and wonderful situations I itemized earlier.

Supporting experimentation and competition in this way is good for end
users, and as @secref{secconv} explains, it's a victory for informed
consent.


@section{On Localization}

Xiden's output is a @racket[read]able list of
@tech/xiden-reference{messages} that one can think of as a
document. @racketmodname[xiden/l10n] translates these documents to
reports in human language. If you store unlocalized messages in a
file, @litchar{xiden show log} will present the file in the user's
chosen language.

At the time of this writing, Xiden only includes English as its used
in the United States. However, a custom launcher may use your own
translations via @racket[format-message].

This is a key design point because this space is about sharing work,
and you can't share work effectively without a way to cross language
barriets.


@section{Versioning}

Xiden versions software using @tech/xiden-reference{editions} and
@tech/xiden-reference{revisions}. Each edition has its own number line
for revisions. Revisions may have names that each map to exactly one
number on an edition's number line.

Xiden defines a query syntax that combines version intervals with
software providers and a specific intellectual
property. @litchar{example.com:http-client:draft:beta:production:ie}
means “The @litchar{draft} edition of the @litchar{http-client}
package made by @litchar{example.com}, from the @litchar{beta}
revision up to but NOT including the @litchar{production} revision.”
The @litchar{i} means “inclusive bound” and applies to @litchar{beta}
because it is the first of the two flags at the end of the query.  The
@litchar{e} means “exclusive bound” and applies to
@litchar{production} because it is the second of the two flags.  For
another example, @litchar{example.com:calendar:small-business:8:8:ii}
matches @italic{exactly} the eigth revision of the small business
edition for a calendar package. This approach allows you to reason
about software in terms of a sociotechnical contract between end-users
and distributors.

Versions are subjective, so you can override how they are interpreted
when prudent. If a set of versions identify software known to be
insecure, the you can dynamically replace the software used by one
version with an acceptable variant. If two names conflict, you can
decide which is canonical. See @secref{names} for more information.

The scheme allows a developer to rewrite code for a new audience
without affecting brand expectations. Users benefit because versions
contain discovery information, making it easier to decide what content
is relevant for them.


@section[#:tag "names"]{On Names}

Names are subjective, so Xiden recognizes no central authority on what
they mean. Any launcher can decide what names are canonical, and what
canonical names mean in a context-sensitive way.

For example, the SHA-1 cryptographic hash function has many
implementations and aliases. One may refer to SHA-1 using strings like
@racket{sha1}, @racket{SHA-1}, @racket{SHA1}, or
@racket{SHA_1}. Additionally, not all implementatons are created equal
in the eyes of an InfoSec expert. You can tell Xiden to see exactly
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
distribution, and how Xiden gives you options when you inevitably want
to control updates in a more nuanced way.

I also explained that Xiden can be used to build and distribute its
own competition as free software.  Any user may form a community
around an individual launcher, or fork a community's launcher without
sacrificing the content it fetches. This model preserves free
association between people.

If you are interested in trying that for yourself, then read
@other-doc[xiden-guide].


@section[#:tag "scenarios"]{Optional: Scenarios}

I've encountered scenarios that have second-, third-, and fourth-
order effects on my teams and clients. If you don't know what some of
these items mean, that's fine. This section acts as an indirect
summary of the kind of problems I consider in Xiden's implementation.

@itemlist[

@item{A DiBOL project in a Subversion repository has one branch per
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
"backup" thumb drive to hit some numbers for the day.}

@item{A catalog that won't store versioned artifacts and won't
guarentee availability for the one it happens to have.}

@item{Name conflicts between unrelated projects on account of their
directory structure.}

@item{A content marketplace that a child can use to hijack
presentations around the world.}

@item{Warehouse crews that @italic{won't} replace their Windows phones.}

@item{Installations that do not say they depend on each other, but
won't work unless you run them in a certain order.}

]
