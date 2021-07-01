When Xiden installs software, it will re-use any outputs that are
already installed based on the exact package query corresponding to
the package. This creates room for conflicts where Xiden will hand you
a link to a previously built directory.

1. Run `racket launcher.rkt do +a a.rkt`
2. Delete the `my-pkg` link
3. Run `racket launcher.rkt do +a b.rkt`

When you do these steps, you'll see that a link called `my-pkg` exists
after the installation of `b.rkt`, but it points to content provided
by `a.rkt`.

This cache is important when versioning because we want a package's
identity to mean something, but sometimes we want to iterate on a
release when the identity is the same. There's a couple of ways to do
that.

You can either edit the definitions to have different names, uninstall
the conflicting package, or edit the launcher to use something like
`(current-package-editor sxs)`.  Stated more broadly: The problem can
be addressed by the end-user or the developer.
