[![](https://img.shields.io/badge/%E2%99%A5-Support%20Ethical%20Software-red)](https://sagegerard.com/show-support.html)
[![Documentation](https://img.shields.io/badge/Docs-Scribble-blue.svg)](https://docs.racket-lang.org/xiden-index/index.html)

Xiden is a dependency manager, and a library for distributing
software.

You can use Xiden as a package manager to bring in dependencies for
your project, but it's so extensible that you can feasibly make an
interface for your own Steam, App Store, or Play Store when you use
Xiden as a back-end. You can also use Xiden as a major component of
continuous integration systems and operating systems.

This does raise two key questions.  Why would anyone do all that, and
how can Xiden be that flexible? The short answer is because Xiden
solves software distribution problems _well_. Think of it as Racket's
answer to Guix.

If you are a release-management nerd, or want to know what
release-management NerdSpeak sounds like, then read [The Long
Answer](#the-long-answer).  In it, I will bore you to tears by
explaining how well the problems were solved.

For a presentation of Xiden during it's alpha stage, see this
[RacketCon 2020 talk](https://youtu.be/bIi-tUzOwdw?t=2330) (Starts at
38:50)!


# The Long Answer

If you judge software by its author: I solve software distribution
problems for large companies. I account for things like differing
technical abilities, who authorizes updates & rollbacks, how
meaningless version numbers actually are, when updates should happen,
how to apply updates in low-service areas, what regions are affected
by that one cache issue, and what surgically-applied exceptions to the
SOP should be made _this time_.

Being in that position made me want to have _and_ eat every cake. The
Holy Grail is the Magic Install Command. The Magic Install Command is
when you, in one command, install the exact software you were thinking
of, at the version range you _meant_, from a party you trust, with
smart answers to prompts baked in, with sane ways to address errors,
and with no distractions like unnecessary files or version conflicts.

There's no way that command exists, right? I think it does. It's just
hard to make because of subjective elements.


## Balancing Security and Convenience

Xiden approaches the Security/UX problem by defining both extremes and
leaving the practical compromise up to configuration.

The first extreme is the built-in zero-trust launcher, `xiden`.  The
`xiden` command takes a firm no-consent (think "Deny All") stance to
any cryptographic hash function, public key, non-OS server
certificate, data transfer budget, intraprocess code execution, memory
quota, time quota, and subprocess spawn attempt. If the user didn't
say you could execute the binary with this SHA3-512 hash, then you
_won't_, dammit. The user must express their entire trust profile in
command line arguments or environment variables, and the launcher will
probably still complain about something they missed. The user also
cannot access some of Xiden's most sensitive configurations through
the default launcher.  The built-in launcher tries to be the best
approach to InfoSec that a program written in a high-level langugage
can be, but it has that horrid Windows UAC problem where you have to
answer a bunch of prompts. It doesn't matter how much they signed away
in that one box with the funny words, most users just smlap<sup><a
href="#1">[1]</a></sup> the prompts away to get things done. `xiden`
doesn't prompt, though, because it's non-interactive.  It just
mechanically says "you didn't allow this scenario" and halts.

A zero-trust extreme is a usabilty nightmare, so the other extreme is
to define a Xiden user as an absolute authority. Despite having many
checks, Xiden does not extend OS-level security for the user running
it as a process. This makes `root` a risky Xiden user, so it's better
to use Xiden for unprivileged installations.  Still, you can leverage
Xiden's implicit trust in its user to write trusted code (think "Allow
All") on top of the "Deny All" rules.

Therein lies the compromise: a custom launcher that flips all of
Xiden's switches and knobs on behalf of a less technical user. A
custom launcher is the user-friendly program that "bakes in" the smart
answers to prompts I mentioned earlier, moving you closer to your own
Magic Install Command. For example, the launcher can embed its own
trusted public keys so it will only install payloads from certain
sources.

But how would you distribute something so particular to your
team/fans/customers/disinterested-mailing-list safely?  With Xiden's
zero-trust launcher! Xiden's rules for data distribution and
verification applies equally to itself, so you can use the "Deny All"
model to distribute custom launchers signed by your private key. And
when an information security incident comes up in the industry, you
can update your launchers to survive this cyberpunk hellscape we call
society.


## Handling Dependency Hell

Xiden is built to handle dependency hell. It detects circular
dependencies, prevents data duplication on a per-file level (even for
SxS installations!), performs atomic file operations, allows the user
to resolve name conflicts, and always fails to a working state. You
can go as far as to make Xiden completely override ranges of
dependencies depending on what's happening to them in the
community. This addresses an oft-overlooked problem in dependency
management: What developers release is often not the same as what a
user expects or allows, now or in the future.

Xiden allows you to reconcile that dilemma in a custom launcher, such
that even if a user _tries_ to install an known insecure or broken
dependency, they'll simply get a safe functional equivalent from the
same trusted party. Put another way, you can leverage the zero-trust
model to only ever allow what you want on your system at the time.


## Distribution Languages

Xiden abstracts over the _solutions_ dependency managers use.  In that
sense, Xiden is both a dependency manager and a library for one. It
can do this because it is written in Racket, so it uses DSLs dedicated
to software distribution problems.

With `#lang xiden/launcher`, the custom launcher DSL, Xiden can be
programmed to behave like NPM, PyPi, `raco pkg`, or like a back-end to
an app store. You can even customize the notation you use for Xiden's
built-in command line interface like switching between SemVer and
Xiden's package query syntax.

Xiden's language for package definitions, `#lang xiden` is a primitive
build specification language that includes ways to declare discovery
information, metadata, and programs that perform archive extraction
and Racket module compilation. Are you missing a feature or support
for a file format?  Put it in the launcher, and distribute that
launcher!


## Versioning

In Xiden, software releases are versioned using editions, and
revisions defined by a provider.  This codifies the distributor and
the intended user as part of a software's identity. This creates an
interesting approach to versioning with a few benefits.

* Versions double as discovery information. It's hard to understand
  why you want `v3.4.581` of something, but it's easy to understand
  that you want the `latest` revision of the `small-business` edition.

* Revisions are defined on number lines, and Xiden supports queries
  that capture intervals with inclusive and exclusive bounds.
  (e.g. `example.com:http-client:beta:production:ie`
  means "The `http-client` package made by `example.com`, from
  the `beta` revision up to but NOT including the `production` revision)

* If a developer wants to throw out everything they did and rewrite a
  program for a new audience, they can do so without affecting the
  brand. Just define a new edition for the same package.

* If a user wants to lock down the interface for a package but
  keep getting security updates, then there are a few options.
  One is to track an edition that meets that promise, or to
  program Xiden to override inputs such that only the desired
  interfaces are used with patches applied.


## Localization

Xiden includes the `xiden/l10n` module for translating Xiden's
machine-readable messages to other human languages.  A Belgian
engineer can accumulate logs from Xiden during offline builds, which
appear to her in Dutch.  When she next establishes an Internet
connection, she emails the logs to her German colleage. The German
colleague can full up the attached log in German.


## Oh my God, will you shut up?

Yep. I'm off the soapbox. I'm biased, and I like my solution. If you
don't want to use Xiden after watching me present it like a grinning
game show host, then please raise an issue in the Issues tab. I find
this domain fascinating and would be happy to meet a challenge that
wasn't considered.

---

###### [1]
A smlap is when you try to touch something but can't decide on a
"smack" or a "slap," so you do a flaccid combination of the two.  It's
that thing that you do to a touchscreen, a keyboard, or a mouse to
convey dismissiveness to the computer that's just doing its job.  If
you've ever been smlapped, it feels as unpleasant as the portmanteu
sounds when you say it out loud.
