When possible, Xiden will re-use any outputs that are already
installed. This implies that installed software acts as a cache in
Xiden. That cache is keyed by _exact package queries_, which creates a
shared namespace for all installed software. When these keys conflict,
Xiden will hand you a link to installed software, thinking that is
what you want. That's often the right thing to do, but we can override
that when prototyping.

Let's look at an example. There are two definitions in this file:
`lub`, and `dub`. They both use the package name `heart`, and default
names everywhere else. Follow these steps to induce a conflict.

1. Run `racket launcher.rkt do +a lub.rkt`
2. Delete the `heart` link. Do _not_ collect garbage.
3. Run `racket launcher.rkt do +a dub.rkt`

You'll see a link called `heart` after Step 3, but it shows `lub.rkt`,
not `dub.rkt`! Step 3 would have created a link pointing to `dub.rkt`
if you collected garbage on Step 2, because then the name would be
free again.

The definitions can either agree to use different names, or a launcher
can force side-by-side installations for all definitions using
`(current-package-editor sxs)`. The problem can be addressed by an
end-user or a developer, which is nice. Installations are also scoped
per-workspace, so you can always get a blank namespace by giving Xiden
a blank state as input.
