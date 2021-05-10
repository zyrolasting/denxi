#lang scribble/manual

@require[@for-label[racket
                    @except-in[xiden/pkgdef #%module-begin]]
         "../shared.rkt"]

@title{Handling Package Conflicts}

When Xiden installs a package, it will re-use any outputs that are
already installed. This creates room for @deftech{package conflicts},
where two different packages share an identity but produce different
output.

You would experience a package conflict as a cache hit. Depending on
the nature of the code leading up to the cache hit, we might respond
differently. This tutorial will cover different scenarios that you can
reproduce on your own, and some tactics for handling them.

The cache considers the provider, package name, edition, revision
number, and output name. If they @italic{all} match, then we get a
cache hit. This would be rare in practice, but it is still
possible. There are ways to solve this in a way that makes sense for a
project.


@section{Scenario: Version Conflict}

Create a package definition that writes a throwaway CSS file.

@racketmod[xiden

(name "css")
(provider "example.com")
(edition "default")
(revision-number 0)
(input "default" (text-source "body { color: red }"))
(output "default" (fetch-input-to-file "default" "styles.css"))
]

Let's install the package using @litchar|{xi do +a defn.rkt -Y '#t'}|.
You'll get a symbolic link with the CSS file.

Now delete the symbolic link, but don't run a garbage collection
pass. Install the package again using the same command, and you'll see
Xiden say that it reused the output. This is because the content was
still available in Xiden's @tech/xiden-reference{state}.

Now let's update the definition to use blue text.

@racketblock[
(input "default" (text-source "body { color: blue }"))
]

Again, delete the symlink but don't collect garbage. Run the
installation again and you'll see that you still have the red
revision. This is expected, and it shows the nature of @tech{package
conflicts}.

In this scenario, we could interpret this as a failure to increment
the revision number. When publishing, a change in content warrants a
change in @tech/xiden-reference{version}.

If you run a garbage collection pass, the cache will be cleared and
you are free to install the new content for the revision.


@section{Scenario: Winning a Namespace Turf War on a System}

You might collide with packages offered by a different provider that
happens to share a name. Whether the other party chose names with the
intent to cause conflicts is not relevant for this section.

You get a support ticket from a user. They tries to install someone's
CSS package, but they see Xiden reused an output with your code
instead. On inspecting the package definition, you see that the other
provider uses the same @racket[provider] name and (coincidentally) all
the same identifying information. Since the user also installed your
software, so they got a cache hit.

Every Xiden installation ultimately defers to the end-user, so you can
instruct to put this in their @tech/xiden-reference{launcher} (Replace
@racket{example.com} with your own provider name).

@racketblock[
(current-package-editor
 (rewrite-conflicting-providers "example.com"))
]

@racket[current-package-editor] is a privileged
@tech/reference{parameter} that alters packages before
installation. @racket[rewrite-conflicting-providers] in particular
checks if a package @italic{would} conflict for a given output, then
adjusts the provider with an increasing one-based numerical suffix
until no conflicts exist for the output.

This is not the only tactic you can use, and it might not be the one
you want. There's always tradeoffs, such as this approach potentially
identifying you as @racket{example.com1}, @racket{example.com2}, etc.
on a user's system.

The point is that package conflicts are resolvable through simple
configuration changes.


@section{Scenario: Subverting the Cache}

The last scenario shows that you cannot disable Xiden's cache, but you
can dodge it.

Let's say we generate a pseudorandom name for every package using
@racket[current-package-editor] in our
@tech/xiden-reference{launcher}.

This makes make package conflicts next to impossible for a
@tech/xiden-reference{workspace}, but it also disables cycle
detection!  Xiden uses the same information for cycle detection that
it does for caches, so this is a risky configuration for general use.

It is, however, useful when you are iterating on a single release
where you need to fix a bug without changing the package's version or
identity. Once the definition works how you expect, you can clean up
your symlinks and collect garbage.
