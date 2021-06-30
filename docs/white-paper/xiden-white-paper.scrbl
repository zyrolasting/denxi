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


@section{Why You Should Care}

One day, Windows @italic{will} install those updates. It doesn't
matter what you are doing at the time, and you don't know what
Microsoft has planned for your poor computer next. That unknown
affects computers spreading to our phones, cars, fridges, and washing
machines.

If your device worked today, then you expect it to work tomorrow. A
good update is therefore like a sunrise: It's expected, it's
necessary, and it doesn't surprise you. A bad software update breaks
something you paid for, and you might not know how to fix it.  While a
failed update is nowhere near as bad as a failed sunrise, your plans
are probably still ruined, and you might wish to avoid future updates.

Part of the problem is people like me, and I don't say that to
self-deprecate. It's just that software teams choose tradeoffs that
impact their users. How can we limit the impact of our biases, and
minimize the chance of interruption for others when we start changing
stuff? I don't think versioning was that helpful of an answer.

It comes down to responsibility and control. If something I control
impacts users and I cannot please everybody, then everybody should
have a way to assert control over parts of distribution. A lot of
release management nerds would gasp and cringe at the idea, and I'll
get to why. All I mean is that it helps when users have a way to take
responsibility for themselves.

Most end-users are not technical, and there are still some powers that
developers need to deliver software to them. That line blurs quite a
bit, so we need a way to adapt to the complex social dynamics that
mess with software distribution. I designed Xiden to help assemble and
distribute its own alternatives, while preserving the end user's right
to understand and change those alternatives. For more on this, see
@secref{competition}.

If you cannot understand what I'm on about, that's fine. I'm a release
management nerd and I care more than I should about boring things.  I
care because the results are @italic{not} boring! We have a problem
with strangers acting like whatever you're doing can't be more
important than their update.

We have confirmation dialogs to collect consent, but consent and
software have a weird relationship.  We have dialogs that act like
”no” doesn't @italic{really} mean ”no.” Worse, when you say “yes”,
things happen that you cannot easily audit.  Updates either hide too
much detail for informed consent, or show so much detail that you
wouldn't want to learn it all in the first place. And even if you had
good reason to deny an update, you don't have the access necessary to
make your own changes. The end result is the same: You click “Yes” to
keep moving, and whatever happens happens.

Most software teams are not trying to break your devices, but it's
easy for them to do so. This is where I have to get technical. Below
is a list of situations I've encountered that lead to software
distribution problems. Each scenario has second-, third-, and fourth-
order effects on how people get through their day.

@margin-note{If you aren't sure what some of these items mean, then
ask the nearest person wearing a @litchar{$ my other PC runs
GNU/Linux} tee. Follow your nose.}

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

I don't have time to explain the countless ways software distribution
can go wrong, but I hope this list shows why there @italic{would be}
countless ways. There is no blueprint to solve every problem, and
Xiden isn't trying to be one. All you need to know is that a bunch of
strangers can break your stuff, they won't always know what they are
doing. You're involved in software distribution whether you like it or
not, so it would be nice if you had something to shield you from the
bad parts while keeping the good parts.

In this paper I explain how Xiden toes the lines that run through each
of the listed situations. I don't present any new ideas. I combine
existing ideas into something predictable and self-evident, because
that's how a sunrise works.


@section{Usage}

Xiden creates directories, and links to those directories.

Installation, updates, rollbacks, or experimental versions boil down
to directories and links. All users (human or machine) get software
from Xiden in this way. You can use Xiden with any project written in
any language, so long as it can resolve links. To uninstall software,
delete the links you don't want and tell Xiden to delete everything
without any incoming links.

I'm hand-waving over a lot of code, because so much can go wrong that
affects the user experience. That's what gives us a lot to talk about
in the following sections.


@section[#:tag "secconv"]{On Security and Convenience}

Like anything else, Xiden can be targeted by nefarious actors. All
software has something called an @deftech{attack surface}, which is
basically a name for all of the ways evil can exploit and control a
system. Xiden's attack surface consists of its configuration, and the
permissions of the user running it.

To mitigate the risks in this area, Xiden includes a zero-trust (think
“Deny All”) launcher called @litchar{xiden}. No installations are
possible with @litchar{xiden}'s default configuration. It takes effort
to make it less safe to use, and it can in fact become @italic{harder
to use} as it evolves. You could argue that it is user-hostile in its
relentlessly bureaucratic nature. It represents security-consciousness
in its most extreme form for a high-level language.

So you run @litchar{xiden do ++install-abbreviated ...} to install
some software. It fires back “Wait, hold on. What cryptographic hash
functions do you trust? Server certificates? How big of a buffer
should I use for data transfers? What is the largest download size you
will tolerate? Can I run your shell? Can I run @italic{a} shell? What
public keys do you trust? Which implementation of SHA-224 were you
thinking of using? Where are you going?”

Notice these are highly-specific questions that any release management
nerd would ask. But most people would--allegedly--simplify this with a
thousand fucking confirmation prompts. If we feel annoyed and
interrupted by simple Yes/No questions, then there's no way anyone
@italic{other} than a release management nerd would enjoy using the
default launcher.

We'd be furious if our phones needed our input on every allowed
contact and local infrastructure before making or taking calls. But
here's the twist: robo-callers cannot spam these phones the way they
do now. Your obsessive ex can't keep texting from different
numbers. The difference is the magic of “Deny All.” IT professionals
will always tell you a firewall should start life allowing
nothing. Allowing too much means risking strangers messing with your
stuff, and we don't want to get stuck in an arms-race. So while it is
annoying to use, @litchar{xiden} only installs software when the
expectations of the user and the software provider are consistent.
That @italic{has} to be true to distribute software well.

As good as a safe launcher can be, we need a path to
user-friendliness. For that, Xiden offers the
@racketmodname[xiden/launcher] DSL to build custom launchers. Custom
launchers are easier to use because they bake in all of the little
annoying--yet important!--decisions that @litchar{xiden} insists you
make. You don't have to expose Xiden's full interface for the 10% of
functionality you and your users need, but users can be confident that
the 10% is carefully considered.

Launchers control Xiden's configuration as privileged code. Since the
configuration is part of Xiden's attack surface, launchers also need
to be protected on the host. Even though it takes effort, it is still
possible to set up commands that are exactly as offensive and careless
as @litchar|{curl --insecure | sudo sh}|. While that's a terrible idea
for production systems, we have to allow for that because we cannot
anticipate every intended use case.

You can distribute custom launchers @italic{using} @litchar{xiden},
the default launcher. Assuming enough care, a specialist would use
@litchar{xiden} to distribute a more convenient alternative to their
communities.  Because the alternative bakes in surgical decisions, the
end-user does not have to know about the protections that apply to
them.  But, they can always look at the launcher, adjust it
themselves, and repeat the process. After all, their consent matters
not just on the matter of software being installed on their devices,
but in the way that it happens.


@section{Handling Dependency Hell}

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

Overridding is powerful because

@itemlist[

@item{it can handle diamond dependencies by programmatically
substituting the four offending inputs with mutually-correct
equivalents before any of them appear on disk. One can do this without
waiting on updates to all four modules from different sources.}

@item{it can replace insecure payloads with patched equivalents, and
to collapse many equivalent dependencies into one. This helps you fix
Racket programs that access non-@racket[eq?] bindings from two
versions of allegedly equivalent modules; and}

@item{it can replace a program that builds a project from source with
a pre-built binary that you already trust.}

]

In brief, where Xiden cannot outright solve dependency hell, it is
designed to allow surgical responses such that a user is never
blocked. And if you get tired of solving those problems over and over
again, just start from a launcher with your favored solutions.


@section[#:tag "competition"]{Following Racket's Pro-Competitive Example}

Xiden is written in Racket. Racket is a programming language, and
Xiden is a software distribution tool. Both are able to adapt in
interesting ways because they are what they easily create. Racket can
make you your own programming language, and Xiden can make you your
own App Store.

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


@section{Localization}

@racketmodname[xiden/l10n] translates Xiden's machine-readable
@tech/xiden-reference{messages} to human language. All Xiden output is
represented as a value, which can be read entirely back by the Racket
reader.

@racketblock[
(call-with-output-file* "log.txt"
  (lambda (to-file) (writeln xiden-log to-file)))
]

The log file may appear in a locale-specific language when viewed
using @litchar{xiden show log}, assuming translated strings are available.

At the time of this writing, Xiden only includes American English.
However, a custom launcher may use your own translations via
@racket[format-message].


@section{Versioning}

Xiden defines a versioning scheme with @tech/xiden-reference{editions}
and @tech/xiden-reference{revisions}. It's easy to know why you'd want
the @litchar{latest} revision of a @litchar{small-business} edition,
so the versioning scheme doubles as discovery information for a target
audience.

Each edition has its own number line for revisions. A revision's name
maps to exactly one number on the line. This way, Xiden supports
queries that capture revision intervals with inclusive and exclusive
bounds. @litchar{example.com:http-client:draft:beta:production:ie}
means “The @litchar{draft} edition of the @litchar{http-client}
package made by @litchar{example.com}, from the @litchar{beta}
revision up to but NOT including the @litchar{production} revision.”

While verbose, this scheme allows a developer to rewrite code for a
new audience without necessarily affecting a brand. Users also benefit
because if they want backwards-compatibility, they may track an
edition created under that premise.

Xiden understands that version numbers are subjective, so you can
override how they are interpreted when prudent. For example, if a set
of versions identify software known to be insecure, you can
dynamically replace those versions in the same way one would redirect
a user from one web page to other.

Users that prefer other schemes may use them in a custom launcher.
You may use Semantic Versioning, dates, or no scheme at all.


@section{Conclusion}

In this paper I explained why you should worry about software
distribution, and explained how Xiden can protect you from strangers
that keep breaking your stuff.

I also explained that Xiden can be used to build and distribute its
own competition, as free software. This creates much more favorable
social situations that allow any user to strike their own balance
between security, convenience, control, and responsibility.

Because software distribution is a difficult problem with subjective
solutions, we need a tool that accepts this reality and helps people
navigate it quickly. That's how Racket helped people become more
accepting of variety in programming languages, because you never felt
like you were losing control when you switched between Racket-powered
languages. I believe that Xiden can bring that same experience to
frustrated users who need options.

If you are interested in trying that for yourself, then read
@other-doc[xiden-guide] to set up Xiden and write a prototype for your
own app store.
