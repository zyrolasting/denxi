When possible, Xiden will re-use any outputs that are already
installed. This implies there is a cache. That cache is keyed by
_exact package queries_. When these keys conflict, Xiden will hand you
a link to an old directory when you expected a link to a new directory.

You can make that happen here:

1. Run `racket launcher.rkt do +a a.rkt`
2. Delete the `my-pkg` link. Do _not_ collect garbage.
3. Run `racket launcher.rkt do +a b.rkt`

When you do these steps, you'll see that a link called `my-pkg` exists
after the installation of `b.rkt`, but it points to content provided
by `a.rkt`. Step 3 would have worked if you collected garbage on Step
2, but then you have the same conflict in the other direction.

The definitions can either change to use different names where they
conflict, or a launcher can use something like
`(current-package-editor sxs)` to force side-by-side installations for
all definitions. Stated more broadly: The problem can be addressed by
an end-user or a developer.
