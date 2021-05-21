[![](https://img.shields.io/badge/%E2%99%A5-Support%20Ethical%20Software-red)](https://sagegerard.com/show-support.html)
[![Scribble](https://img.shields.io/badge/Docs-Scribble-blue.svg)](https://docs.racket-lang.org/xiden-index/index.html)

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
hard to make.

I started by designing a zero-trust security model. _Nothing_ happens
unless the user typed in the means to consent to _exact_
scenarios. Sounds great, but how do you solve that brain-numbingly
horrid problem that Windows UAC had? You know, when you were asked one
question at a time by a single-file line of dialog boxes? It doesn't
matter how much you just signed away in that one box with the funny
words, you're just smlapping<sup><a href="#1">[1]</a></sup> it all
away to get things done.

Xiden approaches the Security/UX problem by defining both extremes and
leaving the practical compromise up to configuration.

The first extreme is the built-in launcher, which takes a firm
no-consent (think "Deny All") stance to any cryptographic hash
function, public key, non-OS server certificate, data transfer budget,
intraprocess code execution, memory quota, time quota, and subprocess
spawn attempt. If the user didn't say you could execute the binary
with this SHA3-512 hash, then you _won't_, dammit. The user must
express their entire trust profile in command line arguments or
environment variables, and the launcher will probably still complain
about something they missed. The user also cannot access some of
Xiden's most sensitive configurations through the default launcher.
The built-in launcher tries to be the best approach to InfoSec that a
program written in a high-level langugage can be, but it's clunky to
use.

The other extreme is that Xiden deems the user controlling it through
a command line or its libraries as an absolute authority. Despite
having many checks, Xiden does not define accounts, so it does not set
OS-level restrictions on the user running it as a process. So with a
little study, you can leverage that power to decide what names are
canonical for any software release. So when you tell it to install
`browser`, you get Brave, because that's the browser you always
install. Except when you are at work, where only Chrome seems to show
all the internal websites smoothly. Oh, and the launcher has its own
copy of public keys so it will only install what came from people you
trust.

You can do all that, and put a simpler interface on top by making a
custom launcher. That's the user-friendly program that "bakes in" the
smart answers to prompts I mentioned earlier. That launcher controls
Xiden, so it can configure Xiden to trust what the launcher
trusts. But how would you distribute something so particular to your
team/fans/customers/disinterested-mailing-list safely?  With Xiden's
built-in launcher! Xiden's rules for data distribution and
verification applies equally to itself, so you can use it to
distribute custom launchers signed by your private key. And when an
information security incident comes up in the industry, you can adapt
your launchers to survive this cyberpunk hellscape we call society.

Xiden is built to handle dependency hell. It detects circular
dependencies, prevents data duplication on a per-file level (even for
SxS installations!), performs atomic file operations, allows the user
to resolve name conflicts, and always fails to a working state. You
can go as far as to make Xiden completely override ranges of
dependencies depending on what's happening to them in the
community. This addresses an oft-overlooked problem in dependency
management: What developers release is often not the same as what a
user expects or allows, now or in the future. Xiden allows you to
reconcile that dilemma in a custom launcher, such that even if a user
_tries_ to install an known insecure or broken dependency, they'll
simply get a safe functional equivalent from the same trusted party.

Granted, you have to know how to program Xiden to do cool stuff like
that. But that knowledge pays off with the ability to set up powerful
platforms, along with the ability to distribute extensions of
itself.

With the custom launcher DSL, Xiden can be adjusted behave like NPM,
PyPi, `raco pkg`, or like a back-end to an app store. You can even
customize the notation you use for Xiden's built-in command line
interface like switching between SemVer and Xiden's package query
syntax. This is possible because Xiden is highly-modular, and it
abstracts over the _solutions_ dependency managers use. In that sense,
Xiden is both a dependency manager and a library for one.  Also, it
just so happens that Xiden cares about a lot of the same problems that
operating systems and continuous integration systems use. Case in
point: Xiden's language for package definitions resembles a build
specification language.

Versioning is another tough problem to consider, and Xiden addresses
it by factoring a target audience as part of a version. Software
releases are categorized under providers and editions, codifying the
distributor of a release and the intended kind of person who intends
to download it. This creates a way to adapt software to new audiences
(breaking changes and all) without disrupting existing users.

Xiden includes localization facilities for translating its messages to
other human languages. Xiden can also print its output as a
machine-readable document. Together, those features allow for
international use.  Arlise, a Belgian engineer, can accumulate logs
from Xiden during offline builds. She reads the logs in Dutch.  When
Arlise next establishes an Internet connection, she can email the logs
to her German colleage. The German colleague reads the same attached
log in German.

With that, I'm off the soapbox. I'm biased, and I like my solution. If
you don't want to use Xiden after watching me present it like a
grinning game show host, then please raise an issue in the Issues
tab. I find this domain fascinating and would be happy to meet a
challenge that wasn't considered.

---

###### [1]
A smlap is when you try to touch something but can't decide on a
"smack" or a "slap," so you do a flaccid combination of the two.  It's
that thing that you do to a touchscreen, a keyboard, or a mouse to
convey dismissiveness to the computer that's just doing its job.  If
you've ever been smlapped, it feels as unpleasant as the portmanteu
sounds when you say it out loud.
