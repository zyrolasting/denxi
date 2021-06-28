#lang scribble/manual

@require[@for-label[racket
                    @except-in[xiden/pkgdef #%module-begin]
                    xiden/package]
         "../shared.rkt"]

@title[#:tag "package-conflicts"]{Handling Package Conflicts}

When Xiden installs a package, it will re-use any outputs that are
already installed based on the @tech/xiden-reference["exact package
query"] corresponding to the package. This creates room for
@deftech{package conflicts}, where Xiden will hand you a link to a
previously built directory.

You, the user, are being treated exactly the same way as any other
dependent requesting a dependency in Xiden's model. I, Xiden, notice
that you requested the @racket{default} output of the @racket[0]th
revision of the @racket{minimal} edition of the @racket{css} package
by @racket{example.com}. I happen to have it ready. Here's a link.
Have a great day.

As you can clearly collect, the cache that causes our conflicts
considers a cacophony of names. A provider name, a package name, an
edition, a revision number, and an output name. Namely. All of these
names are part of an installed directory's identity. If they
@italic{all} match for a package that's about to be installed, then we
get a cache hit.

You can imagine that conflicts would be rare in practice, but they pop
up if you are prototyping your own packages. In general, conflicts are
a problem anytime you expect to install something other than what's
cached.

Depending on the nature of the code leading up to the cache hit, a
Xiden programmer might respond differently. This tutorial will cause
conflicts, while I discuss the everyday scenarios that make them
likely. While we address each conflict, you'll see moving parts that
may help you come up with interesting release strategies.


@section{Scenario: Version Conflict}

Create a package definition called @litchar{defn.rkt} that writes a
CSS file. You need to make sure that every time you change your
deliverable, you change your version somehow.

@racketmod[xiden

(name "css")
(provider "example.com")
(edition "default")
(revision-number 0)
(input "styles.css" (artifact (text-source "body { color: red }")))
(output "default" (keep-input "styles.css"))
]

Let's install the package using @litchar|{xiden do +a defn.rkt -Y '#t'}|.
You'll get a symbolic link with the CSS file.

Now delete the symbolic link, but don't run a garbage collection
pass. Install the package again using the same command, and you'll see
Xiden say that it reused the output. This is because the content was
still available in Xiden's @tech/xiden-reference{state}.

Now let's update the definition to use blue text. Replace the only
@racket[input] line with this one.

@racketblock[
(input "styles.css" (artifact (text-source "body { color: blue }")))
]

Again, delete the symlink but don't collect garbage. Run the
installation again and you'll see that you still have the red
revision. This is expected, and it shows the nature of @tech{package
conflicts}.

Given the scenario I mentioned, we could interpret this as a failure
to increment the revision number, or change the edition. When
publishing, a change in content warrants a change in
@tech/xiden-reference{version}. Failing to do so will induce a
conflict, because for what reason would Xiden expect the output to
change?

But if you delete the symlink and run a garbage collection pass, the
cache will be cleared and you are free to install the new content for
the revision.


@section{Scenario: Interrupted Prototyping}

We'll now cause another package conflict under a different scenario.

Assume you want to quickly test a package definition. You fire up your
text editor, jot this down, save the file, and exit the text editor
faster than the time it will take for you to finish this paragraph.
You don't re-open the file to proofread. Parentheses flow from you
like the Lisp master that we all know you are.

@racketmod[
xiden
(input "hello.txt" (artifact (text-source "Hello!")))
(output "default" (keep-input "hello.txt"))
]

You quickly install the text file that your text editor helped you
express one dimension ago.  You feel power over your life again. You
wonder: Will it grow?

Now let's change the content of the input, just like with the CSS
example before.

@racketmod[
xiden
(input "hello.txt" (artifact (text-source "Hello, world!")))
(output "default" (keep-input "hello.txt"))
]

If you install this modified definition, the text file inside will
still read @racket{Hello!}.

We have a package conflict, yet again. But this time you experience
the conflict as Xiden's @italic{mistake}. You wanted to quickly
iterate on what you typed, because the package's identity is
irrelevant.

The conflict occurred because both package definitions declared no
identifying information, or version. They all fell back to their
default values, because there is no anonymous package.

This scenario is important and likely, because it's useful.  I wasn't
trying to be cute in the way I wrote in the second person.  A
prototyping mindset will quickly encounter a package conflict, so we
need to accomodate it. Let's open up a @tech/xiden-guide{launcher} and
add this line.

@racketblock[
(current-package-editor sxs)
]

@racket[current-package-editor] lets you swap out packages as Xiden
encounters them, but before they are installed. That's a powerful hook
that could be exploited, so it is not configurable by a zero-trust
launcher.  You must start from a @italic{more} privileged Racket
program that you trust to use @racket[current-package-editor], like a
custom launcher.

@racket[sxs] is a built-in procedure that returns a new package with a
unique provider name. This has a neat tradeoff: Package conflicts
become nearly impossible, but it comes at the cost of defeating the
output cache. @racket[sxs] forces Xiden to take "Side-by-Side" (SxS)
installations to the extreme where no output can be cached.

Installations using @racket[sxs] will use more resources. They also
are at risk of getting stuck in a circular dependency.  Think about
it: if one package depends on itself, and you scramble their
identities before processing them, how would you know that they would
cycle?

@racket[sxs] allows you to quickly draft prototype definitions while
keeping the default package identity.  Even if you are collaborating
with other people and firing package definitions back and forth, all
of you would avoid inducing cache hits in one another if you each use
@racket[sxs]. Working this way means needing to delete more links and
run garbage collection passes.


@section{Scenario: End-user Support}

In the event that your software collides with that of a stranger's on
an end-user's PC, you can instruct the user to reinstall with
@racket[sxs] to resolve the conflict. Or, if you are actively
iterating on a release with a customer, leave it in!

In a support context, colliding packages on a client's machine can be
worrisome. Someone could be impersonating your provider name, or your
team made an error in how you distribute package definitions.

One way to handle this is to distribute a launcher that uses a
specialized procedure for @racket[current-package-editor]. It reserves
a unique provider name for your packages, so that when you recognize
an upcoming conflict affecting your users, you can make sure all
relevant packages share the same provider name you control.
