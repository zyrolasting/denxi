#lang scribble/manual

@require["../shared.rkt"]

@title[#:tag "cache"]{Caches}

Xiden uses a cache for package inputs and a cache for package outputs.
Knowing how these caches work gets you the most out of Xiden.

There are two caches because there are two kinds of people who use
dependency managers. The first says “I expect to install the version
of X that I requested, no matter what.” The second says “I expect to
install the version of X I happen to be working on here.”

The first kind of person is served by both caches. The second only
wants one enabled.


@section{Package Input Cache}

Xiden's package input cache is designed such that each blob of data is
expressed on disk exactly once. If you install two entirely different
software packages like a photo editor and a document viewer, the
common inputs between them will appear as symbolic links targeting the
same files or directories.

Package inputs are cached indirectly by the
@tech/xiden-reference{source} from which their bytes come.  That means
if you say an input comes from a particular URL using an
@racket[http-source], then that URL will be checked against the cache
to decide if a download is necessary.

All built-in @tech/xiden-reference{sources} are cached in this way.
You can cause a cache miss by deleting links and collect garbage, or
changing the source used for an input.


@section{Package Output Cache}

Package outputs are cached with the help of @tech{package
queries}. Any installed package output that matches against a package
query is eligible for re-use when selected with the @litchar{do}
command.

Because package queries effectively assign names to outputs, you gain
a risk of @tech/xiden-tutorials{package conflicts}. A package conflict
occur when two package definitions that correspond to the same
@tech/xiden-reference{exact package query} produce different files for
the same output name. Put another way, a package conflict is a cache
hit against the package output cache. The user will experience this
cache hit as a symbolic link pointing to the output of a package that
was already installed.

For the crowd that wants the same output to repeat across
installations, this is great. It makes package queries meaningful
since they will correspond to the same data.

For the crowd that is experimenting on a release, this is less than
ideal. The output cache has to be cleared every time they want to
experiment on the same version of their own packages. In this case
they @italic{expect} the content to change despite having the same
identity for the package.

You can force your way past the output cache to perform a side-by-side
installation of any package. This way new packages will gain a unique
identity, allowing for partially redundant installations. The
installations will still re-use inputs to save space. To learn how to
do this and work with package conflicts in general, see @secref[#:doc
xiden-tutorials "package-conflicts"].
