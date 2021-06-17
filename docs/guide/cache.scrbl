#lang scribble/manual

@require[@for-label[racket/base
                    xiden/source]
         "../shared.rkt"]

@title[#:tag "cache"]{Caches}

Xiden has a cache for package input data and a cache for installed
package outputs. You can remove items from both by deleting links and
collecting garbage.


@section{Package Input Cache}

Xiden's package input cache ensures each blob of dependency data is
expressed on disk at most once. Package inputs are cached by
@tech/xiden-reference{source}.  For example, if you say an input comes
from a particular URL using an @racket[http-source], then a cache key
will be computed from the URL. You can therefore induce a cache miss
by adjusting the source (e.g. adding a useless anchor to an URL).


@section{Package Output Cache}

Package outputs are cached with @tech{package queries}. If a package
output matches against a package query, then that output may be reused
in an installation.

Because package queries effectively assign names to outputs, you gain
a risk of @tech/xiden-tutorials{package conflicts}. A package conflict
occurs when two package definitions that correspond to the same
@tech/xiden-reference{exact package query} produce different files for
the same output name. Put another way, a package conflict is when you
encounter a cache hit for package outputs when you expected a cache
miss. You'll know you have a conflict if you install software, notice
that an output was reused, and your new symbolic link points to
existing output.

For those who wants the same output to repeat across installations,
this is great. It makes package queries meaningful since they will
correspond to the same file distribution.

For those experimenting on a release, this isn't ideal. The output
cache has to be cleared every time they want to experiment on the same
version of their own packages. In this case they @italic{expect} the
content to change despite having the same identity for the package.

You can force your way past the output cache using a side-by-side
installation. This way new packages will gain a unique identity,
allowing for partially redundant installations. The installations will
still re-use inputs to save space. To learn how to do this and work
with package conflicts in general, see @secref[#:doc xiden-tutorials
"package-conflicts"].
