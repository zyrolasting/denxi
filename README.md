[![](https://img.shields.io/badge/%E2%99%A5-Support%20Ethical%20Software-red)](https://sagegerard.com/show-support.html)
[![Documentation](https://img.shields.io/badge/Docs-Scribble-blue.svg)](https://docs.racket-lang.org/xiden-index/index.html)

Xiden is a platform for distributing software.

You can use Xiden as a dependency manager, or as a back-end to your
own Steam, App Store, or Play Store. Xiden is also a viable component
of continuous integration systems and operating systems.

Xiden is this flexible because it solves software distribution
problems _well_. Think of it as Racket's answer to Guix.

If you are a release-management nerd, or want to know what
release-management NerdSpeak sounds like, then read [The Long
Answer](#the-long-answer).  In it, I will bore you to tears by
explaining how well the problems were solved.

For a presentation of Xiden during it's alpha stage, see this
[RacketCon 2020 talk](https://youtu.be/bIi-tUzOwdw?t=2330) (Starts at
38:50)!


# The Long Answer

Windows users sometimes deal with forced updates that either interrupt
you, or break something. A Tesla owner [could not start their
car][tesla] because an update failed. If it weren't for Samsung asking
researchers to audit their [smart refrigerator][], we might have heard
about how the box that keeps your food cold leaked your Google
credentials due to a botched update.

[refrigerator]: https://www.pentestpartners.com/security-blog/hacking-defcon-23s-iot-village-samsung-fridge/
[tesla]: https://teslamotorsclub.com/tmc/threads/car-wont-start-software-update-failed.111866/

This is software distribution done badly.

Racket and Python do better using `raco pkg` and `pipenv`, but they
still have hidden surprises that I wanted to avoid. I made Xiden
because I wanted to see if I could do better. When I managed releases
at work, I account for things like differing technical abilities, who
authorizes updates & rollbacks, how meaningless version numbers
actually are, when updates should happen, how to apply updates in
low-service areas, what regions are affected by that one cache issue,
and what surgically-applied exceptions to the SOP should be made _this
time_. I appreciate the challenges in software distribution, and I'll
cover a few of them here.



## Balancing Security and Convenience

Secure applications are often hard to use because they have a lot of
checks. On the other hand, convenient applications are more fragile
and trusting when they shouldn't be.

Xiden approaches this spectrum by defining a zero-trust (think "Deny
All") launcher, then allowing you to make custom launchers to simplify
what matters to you. If you've ever configured a firewall, then this
will seem familiar.

Xiden allows nothing out of the box, _but_ it assumes its user is
responsible for host- and network-level security. It's best to run
Xiden using an unprivileged user, and incrementally configure it to do
what you want.

But how would you distribute a convenient custom launcher to your
team/fans/customers/disinterested-mailing-list?  With Xiden's
zero-trust launcher! Xiden's rules for data distribution and
verification applies equally to itself, so you can distribute
Xiden-backed extensions to Xiden that have been signed by your private
key. When an information security incident comes up in the industry,
you can update your launcher to survive this cyberpunk hellscape we
call society.


## Handling Dependency Hell

Xiden detects circular dependencies, prevents data duplication on a
per-file level (even for SxS installations!), performs atomic file
operations, and allows the user to resolve conflicts by their own
policy. You will never stay in dependency hell because Xiden has a
well-planned escape route from each layer of said hell.


## Incident Response

Xiden may override dependencies depending on what's happening to them
in the community. If you manage trusted software installations for a
company, you can use Xiden to redirect requests to install a known
vulnerable package to a functionally-equivalent version with a patch
applied.

If a smart person creates collisions in a cryptographic hash function
or cracks a cipher, then you can simply revoke trust in the
compromised system and Xiden will not install any software using it.


## Distribution Languages

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

In Xiden, software versions include the distributor, target audience,
editions, and revisions. Kind of like a book cover. This means
versions double as discovery information. It's hard to understand why
you want `v3.4.581` of something, but it's easy to understand why
you'd want the `latest` revision of a `small-business` edition.

Each edition of software has revisions defined by the natural numbers,
such that a revision's name maps to exactly one natural number. This
makes Xiden supports queries that capture revision intervals with
inclusive and exclusive bounds. So,
`example.com:http-client:beta:production:ie` means "The `http-client`
package made by `example.com`, from the `beta` revision up to but NOT
including the `production` revision.

Undet this approach, a developer can rewrite code for a new audience
without affecting the brand. Users that want backwards-compatibility
can seek out an edition promising that.


## Localization

Xiden includes the `xiden/l10n` module for translating Xiden's
machine-readable messages to other human languages.  A Belgian
engineer can accumulate logs from Xiden during offline builds, which
appear to her in Dutch.  When she next establishes an Internet
connection, she emails a machine-readable log to her German
colleage. The German colleague may print the log in German using
`xiden show log`, because their launcher knows their locale.


## Oh my God, will you shut up?

Yep. I'm off the soapbox. I'm biased, and I like my solution. If you
try Xiden, thank you. If you run into an issue, please raise it in the
Issues tab. I find this domain fascinating and would be happy to meet
a challenge that wasn't considered.
