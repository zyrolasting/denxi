When possible, Xiden will re-use any outputs that are already
installed. This implies that installed software acts as a cache in
Xiden. That cache is keyed by _exact package queries_, which creates a
shared namespace for all installed software. When these keys conflict,
Xiden will hand you a link to installed software, thinking that is
what you want. That's often the right thing to do, but we can override
that when prototyping.

Let's look at an example. There are two definitions in this file:
`lub.rkt`, and `dub.rkt`. They both use the package name `heart`, and
they both produce a file called `sound`. Beyond the content of
`sound`, they do not distinguish themselves any further. We can make
them conflict.

Run `racket launcher.rkt do +a lub.rkt`. You'll see a `heart` link
appear. Run `cat heart/sound`, and you'll see `lub`. Now delete the
`heart` link, but do _not_ collect garbage. If we collect garbage, the
cache would have the name available again.

Now run `racket launcher.rkt do +a dub.rkt`. You'll see the `heart`
link again, but the `sound` file still shows `lub` because of the
cache hit.

These conflicts are also easier to make because we didn't version the
definitions. If they were versioned, they would only conflict if their
versions matched. The same can be said for the provider name and a
self-given name, because a definition's identity consists of all of
these items.

The definitions can either agree to distinguish themselves on at least
one of these items, or a launcher can force side-by-side installations
for all definitions using `(current-package-editor sxs)`. Or, since
installations are scoped per-workspace, you can start a new namespace
by using a new workspace.
