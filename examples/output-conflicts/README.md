Denxi caches installed package outputs. The cache uses _exact package
queries_ as keys. This creates a shared namespace for all installed
software in a workspace directory. If keys conflict, Denxi will hand
you a link to previously installed software. We can override that when
prototyping.

There are two definitions in this example: `lub.rkt`, and
`dub.rkt`. They both use the package name `heart`, and they both
produce a file called `sound`. The definitions differ only in the
content they write to `sound`, so we can reproduce a conflict easily.

1. Run `racket launcher.rkt do +a lub.rkt`. A `heart` link will appear.
2. Run `cat heart/sound`. You'll see `lub`.
3. Delete the `heart` link.
4. Run `racket launcher.rkt do +a dub.rkt`. A `heart` link will appear, but `heart/sound` shows `lub` and not `dub` because of a cache hit.

Here are all of the ways to avoid a cache hit:

- The definitions can distinguish themselves more by declaring different editions, revisions, providers, or package names.
- A launcher can force side-by-side installations for all definitions using `(current-package-editor sxs)`.
- Install each package in its own workspace.
- Collect garbage using `racket launcher gc` before Step 4. This will
  make Denxi delete the installed `heart` package because it has no
  incoming links. That would make the name available again.
