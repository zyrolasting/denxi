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


@section{Why Care About Denxi?}

@(define tesla-link "https://teslamotorsclub.com/tmc/threads/car-wont-start-after-software-update.220433/")

I wrote Denxi to change what I believe is a dysfunctional social
arrangement. Today, tech companies and departments connect directly to
your devices to service them. This is far better than sending out
fleets of CDs, floppy disks, or cartridges. Unfortunately, this level
of access to our homes brought privacy and stability problems.  If we
were still buying software on CDs today, would you let the people who
made those CDs come to your house to watch what you do with your copy?
Would you let them sneak into your home to replace your copy of a CD
with another, with changes you do not understand?  If not, then I have
bad news about all of the applications you are using.

Tech companies will never go back to the old ways of distribution.
There's too much data to sell and corrolate. Meanwhile, Tesla drivers
can't @hyperlink[tesla-link]{start their cars} after a bad
update. That and many other @secref{scenarios} show a toxic pattern,
to which Denxi is a response.

I wrote Denxi because I am tired of businesses and communities
enforcing their legitimate interests at the cost of the people they
claim to serve.

Take YouTube. YouTuve uses ads, marketing research, and other forms of
torture to monetize what would otherwise be freeloading. Even if you
pay to remove ads, you are still constantly monitored. Your experience
and enjoyment of life is not private, and the people who watch you act
as if your transparency is owed to them. Ask them where your data appears
in their infrastructure, and you'll learn that's a secret.

This unbalanced relationship remains the same, whether you use YouTube
or even a free (as in "speech") software offering. I wrote Denxi more
as an immediate reaction to @litchar{raco pkg}, a front end for a
package management system I am actively trying to leave.

But in writing Denxi I learned that YouTube, Netflix, Spotify, Steam,
Roku channels, package managers, launchers, etc. are all economic
middlemen that each grant limited access to some portfolio of
copyrighted content. That would be find if the clients were not
written exclusively by the shippers. To the surprise of those who fail
to read terms, users end up locked out of the supply chain connected
directly to their own devices, and sacrifice control according to the
whims of unknown third parties.

Denxi grew from a specific reaction to @litchar{raco pkg} into an
attempt to abstract over the anthropological act of distributing
content (See @secref{competition}). Denxi sits in the same “spot” as
your copy of Steam, Spotify, or Netflix, but does nothing by
default. It is entirely subservient to your explicit, affirmative
consent regarding its own behavior. With Denxi, you completely control
the receiving side of a shipment on your device, regardless of how the
shipment is done.

The act of getting data from a server to a client is not something the
owner of a server has to do, and it's probably better if they
don't. That means we need a way to rapidly update client-side code, in
a process a non-technical user initiates. Denxi is meant to serve this
use case, such that businesses may no longer use the server-client
model as a way to gain leverage over their own customers.

I designed Denxi such that non-technical users may hire independent
developers to hook up a service to a client in the same way one might
ask an electrician to run a wire behind a wall.  I hope to see more
independent developers make money with Denxi in this way, so that
customer-facing developers are rewarded for serving customers
directly.

Under Denxi, anyone can provide a client that reflects what the
client's owner personally allows for any device that supports
Racket. The client's source code would be small and easy to audit,
because it primarily contains trust information. This decouples the
shipping side from the receiving side, and places responsibility for
receiving directly on the user. Any client written with Denxi can be
freely and easily swapped with another. If you need to use a custom
protocol, the implementation of that protocol can be shipped to your
device using Denxi as well.

Denxi is not a package manager, but it can easily create package
managers. This is because Denxi doesn't favor any one idiom, protocol,
or server. It does, however, understand dependency management and all
of the nastiness that comes up when trying to ship software from one
place to another. It also understands subjectivity, in that a
dependent's expectations of a dependency are not the same as a
dependency's expectations of a dependent. Servers and clients can
behave as they normally do, but the end-user has irrevocable control
over all aspects of how data arrives on their system. The shipper is
assumed to have no right, obligation, or excuse to comment on the
user's boundaries.

This is non-negotiable.


@section[#:tag "secconv"]{On Security and Convenience}

Denxi includes a zero-trust launcher called @litchar{denxi}. No
installations are possible with @litchar{denxi}'s default
configuration. It takes a complicated command line to use. In fact,
@litchar{denxi} becomes @italic{harder to use} over time because it is
defined as the worst user experience.

I did this so that the most security-conscious use of Denxi has a
formal identity. That makes it possible to generate a simpler CLI in
terms of details the user does not want to consider.

Denxi's attack surface consists of a single process and its runtime
configuration. The zero-trust defaults make it harder to accidentally
open vulnerabilities and create liabilities as a provider. The user
must open their own security holes.

The @racketmodname[denxi/launcher] DSL builds custom launchers that
represent a specific, subjective line between security and
convenience. Custom launchers are easier to use because they bake in
all of the little annoying---yet important!---decisions that a
zero-trust launcher requires. This protects users from answering a
long line of annoying confirmation prompts, like Windows Vista did in
their early rollout of UAC. But launchers encode more exact boundaries
about what you trust. You don't have to expose Denxi's full interface
for the 10% of functionality you need, but you can be confident that
the 10% is defined in terms of your interests.

You organize communities by sharing custom launchers @italic{using}
@litchar{denxi}. A custom launcher bakes in surgical decisions and
presents a convenient interface, and @litchar{denxi} provides an
unambiguous channel through which others can audit how that launcher
is distributed.

Since a Denxi launcher is just text, an end user can edit it
themselves to repeat the whole process.  You don't need to wait for a
package manager's developer to stop using SHA-1 if you can revoke
trust in SHA-1 yourself. Since every launcher can represent a
communual consensus or an individual preference, each user can respond
to security incidents independently.


@section[#:tag "competition"]{Ecosystem-Oriented Ecosystem}

Denxi is written in Racket. Racket is a language-oriented programming
language, which means that the surface syntax and compiler are both
extensible using the same language they compile. I can switch between
a logic language and a markup language and keep the same package
manager (@litchar{raco pkg}), documentation generator (@litchar{raco
scribble}), and so on.

The opposite is not true. Racket is a language-oriented langauge, but
it does not have an ecosystem-oriented ecosystem. The result is that
you cannot switch to a different toolchain without in some way
isolating yourself from the Racket community. Denxi, like Racket helps
you create and prototype alternatives to itself. Racket makes you your
own programming languages, and Denxi makes you your own middlemen. A
Racket+Denxi stack is capable of capturing both the technical and
social context required to build software, because the way
contributors share content is part of the source code.

Denxi is built on the assumption that tools like it are going to keep
proliferating, and that we should have more that are completely
beholden to what a user expects from it.

Denxi is free software. When Denxi launchers proliferate on your
system, you have say over how they @italic{all} work across various
@secref{scenarios}.


@section{Localization}

You can't share work effectively without a way to cross language
barriers. To aid translation, Denxi's output is a @racket[read]able
list of @tech/denxi-reference{messages} that one can think of as a
human language-independent document. @racketmodname[denxi/l10n]
translates these documents to reports in a specific human language. If
you store unlocalized messages in a file, @litchar{denxi show log}
will present the file in the user's chosen language.

At the time of this writing, Denxi only includes English as it's used
in the United States. Go figure. However, a custom launcher may use
your own translations.


@section{Versioning}

Names are given for subjective reasons, so Denxi recognizes no naming
authority outside of the end-user. Canonicalization favors the
language of the user.

Denxi has no canonical versioning scheme, because versions impose a
way of identifying software that they user could consider irrelevant.
Instead, Denxi gives all installed or downloaded data a user-defined
identity in terms of a developer's untrusted claims about the data.

If you tell me you are sending me version @litchar{9.4.1} of
@litchar{better-css}, and experience it as the latest tested version
of a CSS library for my project, then your software is simply
@litchar{css-lib} according to me.

Additionally, the SHA-1 cryptographic hash function has many
implementations and aliases. One may refer to SHA-1 using strings like
@racket{sha1}, @racket{SHA-1}, @racket{SHA1}, or
@racket{SHA_1}. Additionally, not all implementatons are created equal
in the eyes of an InfoSec expert. You can tell Denxi to see exactly
one of the aforementioned names as canonical, and bind that name to an
implementation you trust. The rest may be defined as aliases for the
canonical name.

The same applies to the names of parties and packages. If you
encounter two packages called @litchar{uri}, you may designate one as
canonical or install them side-by-side.

This design is powerful because it means naming conflicts only happen
when a user allows them to happen.


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

@item{Synology's DS220j started notifying users of a software update that
requires more memory than the unit can possibly hold.}

]
