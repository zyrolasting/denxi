#lang reader "../document.rkt"

@title[#:style '(toc)]{Denxi White Paper}
@by-slg

@define[sh-link "https://github.com/zyrolasting/denxi/tree/master/examples/04-self-hosting"]

Denxi is a programming model for shipping and receiving authenticated
data across ecosystems and untrusted I/O devices. Denxi's favors the
receiving party's rules in all matters of trust and safety, including
integrity, authentication, and resource limits.

This white paper focuses more on the intentions behind Denxi without
repeating details from technical documentation. For all documentation,
see @other-doc[denxi-index].


@section{Motivation}

@(define tesla-link "https://teslamotorsclub.com/tmc/threads/car-wont-start-after-software-update.220433/")

Strangers connect directly to your devices and act like they have the
right to do whatever they want while there. Denxi is my attempt to end
that dysfunctional arrangement, regardless of what terms of service
apply at the time.

We need a world where businesses and open source communities no longer
perform destructive automatic updates or telemetry without the user's
ongoing, explicit, informed, and revocable consent.

To this end, Denxi serves two objectives:

@itemlist[
@item{Grant end-users all choices in data transfers and dependency
management, starting from a position of zero trust.}

@item{Reduce the cost of authoring package managers, downloaders,
CI/CD systems, and other software used to manage data governed by some
set of conventions or idioms.}
]

If a modern package manager were a delivery driver, it would walk into
your house to unpack the box, buy the missing batteries with your
resources, wait for @italic{another} delivery driver to bring/unpack
the batteries, only to install a product in a dedicated location.
This is a highly-invasive operation of unknown duration, that favor's
the shipper's policies about what they can do with other people's
devices. Normally the complexity involved is allowed by a single "Yes"
or "I agree," even when the consenting party does not want to consent
to @italic{everything} about an installation.

If Denxi was a delivery driver, it will ship data to you according to
your rules, and then leave the package at your door with a relevant
toolbox. The tools include a explicit canonicalization model for
managing discovery information, delayed assignment of dependency
relationships, and solutions to dependency hell.


@section{Normative Vision}

I see package managers, streaming apps, indeed all clients as personal
tools tailored to the individual by independent developers. Updating
an app like Netflix is something you either do yourself, or hire a
neutral developer to do for you as a task.

It's better to live in that world, than one where the only way to have
an app like Netflix is to allow Netflix to maintain direct access to
your devices using their own code.


@section{Summary of Benefits}

@itemlist[
@item{Denxi enforces zero-trust principles to avoid
@hyperlink["http://www.ranum.com/security/computer_security/editorials/dumb/"]{many
problems} that explicit, informed, and willful consent would otherwise solve.}
@item{You can manage dependencies for any project or system that runs Racket.}
@item{You can override all aspects of any software deployment.}
@item{If you don't like Denxi itself, you can use it to create and distribute an alternative to itself.}
@item{@litchar{npm} users gain more safety checks to protect your system from an ecosystem gone mad.}
@item{(@litchar{raco pkg} users) You won't mutate your Racket installation when installing software.}
@item{PLaneT users may install multiple versions of a project without generating non-@racket[eq?] bindings.}
@item{Guix/Nix users may expect cross-platform support.}
@item{Users of any package manager or launcher may use Denxi to escape
the limitations imposed by their community.}
]




@section{Respectful Distribution}

We've evolved past mailing out fleets of CDs, floppy disks, or
cartridges. Unfortunately, we grant businesses and open-source
communities a terrifying level of access to our personal lives.

If we were still buying software on CDs today, would you let the
people who made those CDs come to your house to watch what you do with
your copy?  Would you let an Amazon delivery driver walk into your
home to replace an old product with a better version?  If not, then I
have bad news about your apps.

Tech companies will never go back. There's too much data to sell and
corrolate, and few outside of specialized research fields have to pay
to ship physical media. Meanwhile, some Tesla drivers can't
@hyperlink[tesla-link]{start their cars} after a bad update. That and
many other situations show that something is seriously wrong with the
logistics of software. See @secref{scenarios} for more examples.

I wrote Denxi because I am tired of businesses and open source
communities enforcing their legitimate interests at the cost of the
people they need to serve. I'm tired of what appears to be a willfull
dismissal of the end-user's interests when pushing updates, as if
users cannot be trusted to represent themselves as receivers of code
and data that might not even work with their hardware.

I'm also tired of watching talk like they are the leaders of
innovation, as if everyone else can only ever be their followers.
That issue goes beyond distribution, but distribution is how a
business executes their will using software. I will die before I
appease the ego and entitlement of creepy unlicensed professionsals,
but before death, I aim to do what I can to give all power over the
logistics of software to the users.

Most users are not technical, so we developers tend to assume that
users will quickly cower from anything not immediately convenient.
But I know people better than that. People want their problems solved,
but are not afraid to work when they understand what's at stake. Right
now, users trust developers too much. Users trust me with their
experience and enjoyment of life every time I deploy a service for the
survellience-centric business paying me to do so.  Now, businesses
seem to expect your data and ongoing participation in their
operations. Terms of service sit inert without adapting to the
individual, confident that enough users will agree to anything. Who do
you know who acts like so much is owed to them when you want to play a
video game or read an article? Would you invite these people to your
house? Ask a U.S. business where your data appears in their
infrastructure, and you'll quickly learn the nature of your
relationship.

The unbalanced relationships remains the same, whether you use YouTube
or even a free (as in "speech") software offering. I wrote Denxi
initially as an immediate reaction to @litchar{raco pkg}, a front end
for a package management system that I did not want to use.

In writing Denxi I learned that YouTube, Netflix, Spotify, Steam, Roku
channels, package managers, etc. are all economic middlemen that each
grant limited access to some portfolio of copyrighted content. That's
fine, but the clients are written exclusively by the shippers.
Imagine if a factory hooked up a conveyer belt to your living room
window. That's what software updates are like. Maybe the convenience
is worth the implications to some, but I can't stand it, personally.

To the surprise of those who fail to read terms, users end up locked
out of the supply chain connected to their own property. We've
sacrificed control according to the whims of third parties for too
long. It has to stop, and I'm not going to wait on a law to stop it
for me.

Denxi sits in the same “spot” as your copy of Steam, Spotify, Netflix,
etc. All of those apps are ultimately clients. However, Denxi does
nothing by default. It trusts nothing by default.  It is entirely
subservient to your explicit, affirmative consent regarding its own
behavior. 99% of your time using Denxi is telling it what it is
allowed to do, and 1% of the time is telling it to carry out a task.
That's the cost. The benefit is that you completely control the
receiving side of a data shipment targeting your device, regardless of
how the shipment is done. It is a border for your device at the level
of all I/O, designed with an uncompromising lack of trust in the
strangers that think you owe them consent. You know, like a creep
would.

Those with a product or service must operate within Denxi's semantics,
because Denxi users control all aspects of I/O for a process under
those same semantics. Denxi's source code is under the AGPLv3 license,
to limit the extent to which invasive corporate interests might co-opt
Denxi for their own ends.

If Denxi were well-adopted---which I expect it won't be, because it
works---terms of service would need to @italic{ask you} to configure
Denxi to make their service function on your system. That obstacle is
intentional, because a checkbox saying “I Agree” is not enough. Denxi
makes your consent explicit in code, so that no other code can presume
on it.

How else can we enforce respect for the user? It sure can't be asking
people to read. A share of most national populations are illiterate,
and some cannot think more than a few steps ahead. But we are all held
equally accountable to terms of service, and that would suggest that
there needs to be a better guard against creepy interests than a
checkbox and a text wall.

Thankfully, no terms I've seen as a consumer have obligated me to feed
correct data to a service on an ongoing basis. I figure no one will
mind if I help others opt-out of something they didn't really agree to
do.



@section{Economic Justification}

It's not enough to complain about a bad social situation.  People are
driven by money, and in my experience, they won't do the right thing
without being paid. So here's a demand for which others can offer a
supply.

Denxi pushes logistical responsibilities to the client, which reopens
the discussion for how a non-technical user manages their own updates.

I designed Denxi such that non-technical users may hire independent
developers in the same way one might ask an electrician to run a wire
behind a wall. The goal is to give independent developers a way to
make money with customer-facing work. This use of Denxi is meant to
incentive smaller-scale operations to share content without depending
on large platforms to host specialized content like video games or
music. DRM may still exist, but a user must consent to its use through
collaboration with an---ideally neutral---developer.


@section[#:tag "secconv"]{On Security and Convenience}

Denxi defaults to a zero-trust default configuration, making it hard
to use. In fact, Denxi becomes @italic{harder to use} over time
because it cannot presume on a user's experience as it gains features.

I did this so that the most security-conscious use of Denxi has a
formal identity. While it is hard to use Denxi out of the box, it is
easy to generate a CLI in terms of details a target-audience does not
want to consider.

Denxi's attack surface consists of a single process and its runtime
configuration. The zero-trust defaults make it harder to accidentally
open vulnerabilities and create liabilities as a provider, because it
is up to a user to audit and accept the conditions implied by a
launcher.

The @racketmodname[denxi/launcher] DSL builds custom launchers that
represent a specific, subjective line between security and
convenience. Custom launchers are easier to use because they bake in
all of the little annoying---yet important!---decisions that protect a
user's interests. This also protects users from answering a long line
of annoying confirmation prompts, like Windows Vista did in their
early rollout of UAC.

You organize communities by sharing custom launchers @italic{using}
Denxi. A custom launcher bakes in surgical decisions and presents a
convenient interface, and Denxi itself may distribute the launcher
with authentication and integrity checking.


@section[#:tag "competition"]{Ecosystem-Oriented Ecosystem}

Denxi is written in Racket. Racket is a language-oriented programming
language, which means that the surface syntax and compiler are both
extensible via Racket. I can switch between a logic language and a
markup language and keep the same package manager (@litchar{raco
pkg}), documentation generator (@litchar{raco scribble}), and so on.

The opposite is not true. Racket is a language-oriented langauge, but
it does not have an ecosystem-oriented ecosystem. The result is that
you cannot switch to a different toolchain without in some way
isolating yourself from the Racket community. This caused me to work
on Denxi for several years, without access to the libraries that make
Racket more useful. It's worth it, because Denxi implements an
ecosystem-oriented ecosystem.

To understand what I mean, think of what Racket can do.  Racket helps
you create and prototype other languages, including alternatives to
itself. Denxi is the same way, in that it helps you create and
prototype your own tools for another ecosystem. A Racket+Denxi stack
is capable of capturing both the technical and social context required
to share work. The social contract is expressed in source code.

Denxi is built on the assumption that tools like it are going to keep
proliferating, and that we should have more that are completely
beholden to what a user expects from it. Just like how Racket argues
that there aren't @italic{enough} programming languages, Denxi argues
that there aren't enough apps that connect you to content. The
question is: who is going to control that growth? You, or a business
that doesn't seem to care about what they do to you?

Under Denxi, anyone can provide a client that reflects what the
client's owner personally allows for any device that supports
Racket. The client's source code is kept small, to keep it easy to
audit. Any client written with Denxi can be freely and easily swapped
with another. If you need to use a custom protocol, the implementation
of that protocol can be shipped to your device using Denxi as well.
Denxi can ship itself, and its own extensions, without any dependency
other than Racket.

Denxi is not a package manager, but it can easily create package
managers. This is because Denxi doesn't favor any one idiom, protocol,
or server. It does, however, understand dependency management and all
of the nastiness that comes up when trying to ship software from one
place to another. It defines convenient solutions in terms of granular
abstractions that know how to handle corner cases.

Denxi also understands subjectivity, in that a dependent's
expectations of a dependency are not the same as a dependency's
expectations of a dependent. Servers and clients can behave as they
normally do, but the end-user has irrevocable control over all aspects
of how data arrives on their system. The shipper is assumed to have no
right, obligation, or excuse to comment on the user's boundaries.

Since a Denxi launcher is just text, an end user can edit it
themselves to either aid their own community, or leave a bad one. You
don't need to wait for a package manager's developer to stop using
SHA-1 if you can revoke trust in SHA-1 yourself. Since every launcher
represents either a consensus or an individual preference, users can
respond to incidents on their own, together.

One can argue that we can already self-organize, but we cannot do so
cheaply. Denxi's goal is to make self-organization cheap, such that if
I ever spend 5 years building a community, you can still leave and
carry on in a different community within seconds. Nothing I know of
makes that easy, and I suspect only an ecosystem-oriented ecosystem
can.


@section{Localization}

You can't share work effectively without a way to cross language
barriers. To aid translation, Denxi's output is a machine-readable
document. @racketmodname[denxi/l10n] translates these documents to
reports in a specific human language. If you store unlocalized
messages in a file, @litchar{denxi show log} will present the file in
the user's chosen language.

At the time of this writing, Denxi only includes English as it's used
in the United States. I'm American, so go figure. However, a custom
launcher may use other translations to expand Denxi's cultural reach.


@section{Names, Versions, and Canonicalization}

Names are given for subjective reasons, so Denxi recognizes no naming
authority outside of the end-user. This design decision fundamentally
differs from most package managers.

For one thing, it makes versions useless for developers except for
internal communications. All versions are made-up terms in a
developer's language, and Denxi forbids developers from appropriating
a user's system with their own vocabulary.

Denxi has no canonical versioning scheme, because versions impose a
way of identifying software that users might consider irrelevant.
Instead, Denxi gives all installed or downloaded data a user-defined
identity in terms of a developer's untrusted claims about the data.

Canonicalization favors the language of the user. If you tell me, a
Denxi user, that you are offering version @litchar{9.4.1} of
@litchar{better-css}, that does not change the fact I experience it as
the latest tested version of a CSS library for my project. Your
software is simply @litchar{css-lib} according to me. If I wish to
track versions, then that's my business. It's my system, not yours.

Note that this arrangement is different than other package managers.
Normally if you ask a package manager to get you the "latest" version
of something, the semantics depend on available artifacts and a
versioning scheme defined by a server. Here, asking for the "latest"
version of something uses rules defined by the @italic{client}.

We see this design decision appear in Denxi's use of cryptographic
hash functions (CHFs). CHFs have many implementations and aliases. One
may refer to SHA-1 using strings like @racket{sha1}, @racket{SHA-1},
@racket{SHA1}, or @racket{SHA_1}. Additionally, not all implementatons
are created equal in the eyes of an InfoSec expert. You can tell Denxi
to see exactly one of the aforementioned names as canonical, and bind
that name to an implementation trusted by the client. The rest may be
defined as aliases for the canonical name.

The same applies to the names of parties and packages. If you
encounter two packages called @litchar{uri}, you may designate one as
canonical or install them side-by-side.

This design is powerful because it means naming conflicts only happen
when a user allows them to happen. Software gains an identity defined
by only the party affected by the software.


@section{Conclusion}

In this paper I explained why you should worry about software
distribution, and how Denxi gives you options when you inevitably want
to control updates in a more nuanced way.

I also explained that Denxi can be used to build and distribute its
own competition as free software.  Any user may form a community
around an individual launcher, or fork a community's launcher without
sacrificing the content it fetches. This model preserves free
association between people, and makes it difficult for unethical
distribution models to survive.

If you are interested in reaping these benefits for yourself, then
read @other-doc[denxi-guide]. I am also available for hire for those
seeking specialized Denxi programs.


@section[#:tag "scenarios"]{Addendum: Scenarios}

I've directly observed scenarios that have second-, third-, and
fourth- order effects on my teams and clients.

Here I recite concrete causes of distribution problems that I expect
Denxi would address.

@itemlist[

@item{A DIBOL project in a Subversion repository has one branch per
customer. When one customer gets a new feature, some other customers
want the same feature. None of their branches merge automatically.}

@item{A stubborn developer with too much access pushing a breaking
change to a production system and refusing to take calls.}

@item{An update script that pulls unverified archives from an
executive's personal DropBox. It extracts the archive, clobbering
itself and other files in the running project.  The code controlled
large machines that could hurt somebody in over 40 locations in the
continental United States. No version control was available, so each
implementation is manually tweaked by on-site operators when the
DropBox deployment breaks. This script partially defines the flight
schedule of an operations manager.}

@item{A system that may or may not automatically reinstall a specific
application that day. If it does reinstall, we cannot predict what
version it would be.}

@item{A pile of Bash scripts that each expect files in magic
locations. The proper order of scripts is not clear, and calling the
wrong one means contractors can't submit work that day.}

@item{A marketing team that controls how Canadians and Australians
experience the corporate website, but not Puerto Ricans.}

@item{Programmers servicing unsupervised systems over TeamViewer to
get around security boundaries installed by DevOps.}

@item{Managers knowingly revering a patch by moving files from their
“backup” thumb drive to replace Python scripts in a folder they
happened to memorize. They do this every time they need to hit some
numbers for the day.}

@item{A catalog used by @litchar{raco pkg} that won't store versioned
artifacts, and won't guarentee availability for the one it happens to
have.}

@item{Name conflicts between unrelated projects on account of their
directory structure. Also a @litchar{raco pkg}-ism.}

@item{A content marketplace that a child can use to hijack
presentations in primary schools around the world.}

@item{Warehouse crews that @italic{won't} replace their work Windows
phones.}

@item{Programs that do not say they depend on each other, but won't
work unless you load them in a certain order.}

@item{In late 2021, Synology's DS220j started notifying users of a
software-compatible update that requires more memory than that
(non-upgradable) model holds.}

]
