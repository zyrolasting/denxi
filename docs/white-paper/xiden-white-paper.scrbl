#lang scribble/manual

@require[@for-label[racket/base
                    @except-in[xiden/launcher #%module-begin]]
         "../shared.rkt"]

@title[#:style '(toc)]{Xiden White Paper}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

@define[sh-link "https://github.com/zyrolasting/xiden/tree/master/examples/04-self-hosting"]

This document covers software distribution topics handled by Xiden,
and the thinking behind Xiden's approaches.

For all documentation, see @other-doc[xiden-index].


@section{Why You Should Care}

Software updates don't always go well. One day Windows forces a
restart that interrupts productive work, and a Tesla won't start
because an update failed.

Obviously we expect our devices to work, but we can't know if they
will continue to do so without control over how they install
software. Legitimate tech companies and open source communities are
(for the most part) not trying to break your devices, but it's easy to
do so because of things like

@itemlist[

@item{a DiBOL project in a Subversion repository with one branch per
customer.}

@item{an update script that pulls unverified archives from an
executive's personal DropBox. It extracts the archive, clobbering itself
and other files in the running project.  No version control was
available. The code controlled a large machine that could hurt somebody.}

@item{a system that may or may not reinstall a specific application
that day. If it does reinstall, we cannot predict what version it
would be.}

@item{programmers servicing unsupervised systems over TeamViewer.  An
operator would allow in whoever was connected and break for lunch.}

@item{managers knowingly undoing a patch by moving files from their
"backup" drive over the working copy so that they "know it will work
right" when they try to hit some numbers for the day.}

@item{a catalog that won't store versioned artifacts and won't
guarentee availability for any of them.}

@item{countless production systems that allow any employee to see
personal information or stop services.}

@item{name conflicts between unrelated projects on account of their
directory structure.}

@item{package installations that do not depend on each other, but will
still only work when run in a certain order.}

]

These are either distribution problems, or situations that lead to
them. Each item has second-, third-, and fourth- order effects on
people and how they interact. I can't cover the countless ways
software distribution can go wrong, but I @italic{can} say two things.

First, Xiden is to distribution what Racket is to language-oriented
programming. I don't say that to mean Xiden is exciting and new, I say
that to mean that Xiden is what it creates. Beyond that, I hope you
are never surprised by what Xiden does.

Second, Xiden gives all decision making power to its user.  By
default, Xiden does nothing without explicit consent. Those who feel
like they don't control their own devices might find peace of mind
using Xiden.


@section{Usage}

Xiden creates symbolic links to atomically-constructed directories at
the request of any dependent, human or program. Updates, rollbacks,
and installations all entail making a link to a directory. It also
means that any program installing software using Xiden can access
dependencies by resolving links on the file system.

Links act as references to installed data. Deleting links to software
you no longer need can make files eligible for garbage collection, if
no links point to them anymore.



@section{On Security and Convenience}

Xiden includes a zero-trust (think “Deny All”) launcher, where no
installations are possible under the default configuration. It takes
effort to make the default launcher less safe to use.

A user may configure the default launcher using command line options
or environment variables. Alternatively, one can use the
@racketmodname[xiden/launcher] DSL to build a custom launcher. In any
case, the user builds a precise configuration that allows only
specific scenarios. If you've ever configured a firewall like pfSense,
then this will seem familiar.

Custom launchers are expected to be easier to use because they include
trust in exact public keys, cryptographic hash functions, server
certificates, data transfer safety limits, and executables.  This
implies that Xiden trusts its own launchers, and that users are
responsible for securing those launchers on their hosts. Altogether,
Xiden allows the user to define their own compromise.

Assuming trust in host-level dependencies, Xiden's attack surface
consists of the launchers, @litchar{XIDEN_}-prefixed environment
variables, and the permissions of the OS user executing processes.

@section{Self-Host, Stand Out}

In general, Xiden's @tech/xiden-guide{launchers} define a interface,
what to trust, and any relevant conventions.

Xiden's rules apply equally to itself, so you can distribute custom
launchers using Xiden's default launcher. Others can download your
launcher using Xiden, then switch to your launcher to install your
software.

Imagine Alice writes a store client using Xiden. She sends the client
to Bob using Xiden. Bob verifies the launcher came from Alice, and can
now install Alice's software. He can also react to information
security incidents on his own. For example, he can revoke trust in a
cryptographic hash function trusted by Alice's launcher.

You can write an alternative to Xiden using Xiden, just like you can
write an alternative to Racket using Racket. The idea is to have Xiden
save time in the same way Racket does, while approaching alternatives
in the same interesting way.

To understand what that means, remember that Racket is among hundreds
of programming languages. Racket stands out because it saves time
writing and switching to other computer languages. @italic{It is what
it easily creates}. If you have Python but not PHP installed,
switching to PHP means installing a certain version of PHP. It also
means remembering to use a compatible version of @litchar{composer}
and not @litchar{pipenv}, and remembering to reference the right
manual.  This is because neither Python or PHP were designed to
naturally integrate with their own future competition. At its best,
Racket reduces the effort of changing languages down to the decision
to do so.

Similarly, Xiden is another name to add to tens of options, including
Flatpak, Guix, Nix, Pacman, Pipenv, PLaneT, @litchar{raco pkg}, Snap,
Yarn, and Yum. If one implements each of these as Xiden launchers,
then the cost to switch between launchers in Xiden is similar the cost
of switching languages in Racket. I don't intend to reinvent these
programs, but you can use Xiden to distribute your Xiden-based
derivatives with your own conventions and servers. It isn't just a
framework because, again, it is what it creates. That makes it easy
for us to experiment with ways to share work. Consider the
@hyperlink[sh-link]{launcher that installs an isolated instance of
Racket with an updated copy of Xiden}. That copy of Xiden can in turn
reproduce the build that made it.



@section{Handling Dependency Hell}

Like Guix, Xiden uses functional programming principles. Any
dependency is viewed as an argument to a function that builds
dependent software.

This model helps Xiden detect circular dependencies and limit file
duplication, even within SxS installations.

Some examples of dependency hell have no easy answer. Conflicting
identities, diamond dependencies, availability of vulnerable software,
and generative bindings from functionally-equivalent programs can
thwart even the most robust systems. Most developers make judgement
calls to get past these problems, and then caution users against
incompatible practices. Xiden lets users choose and change their own
reaction to the underlying problems, and the scenarios that only seem
to appear in practice.

For example, Xiden allows users to decide what names are canonical in
the event of a conflict. If two programmers claim the name
@litchar{john.doe} and try to distribute software under all the same
names, an end user may still distinguish the John Does' contributions
locally.

Most other forms of dependency hell are addressed using Xiden's
privileged overriding features
(e.g. @racket[current-package-editor]). Because dependencies are
viewed as arguments to a function, one can swap them out.

Overrides can handle diamond dependencies by programmatically
substituting four offending inputs with mutually-correct
equivalents. One can do this without waiting on updates to all four
modules from different sources.

Overriding also allows you to replace insecure payloads with patched
equivalents, and to collapse many equivalent dependencies into
one. This helps you fix Racket programs that access non-@racket[eq?]
bindings from two versions of otherwise equivalent modules.

In brief, where Xiden cannot outright solve dependency hell, it is
designed to allow surgical responses such that a user is never
blocked.


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

Xiden defines a versioning scheme with @tech/xiden-guide{editions} and
@tech/xiden-guide{revisions}. It's easy to know why you'd want the
@litchar{latest} revision of a @litchar{small-business} edition,
so the versioning scheme doubles as this discovery information.

Revisions are defined untimately by a set of non-negative integers
corresponding to an edition, such that a revision's name maps to
exactly one such integer. This makes Xiden supports queries that
capture revision intervals with inclusive and exclusive bounds. So,
@litchar{example.com:http-client:draft:beta:production:ie} means “The
@litchar{draft} edition of the @litchar{http-client} package made by
@litchar{example.com}, from the @litchar{beta} revision up to but NOT
including the @litchar{production} revision.”

This scheme allows a developer to rewrite code for a new audience
without affecting the brand. Users also benefit because if they want
backwards-compatibility, they track the edition created under that
premise.

Users that prefer other schemes may use them in a custom launcher.
