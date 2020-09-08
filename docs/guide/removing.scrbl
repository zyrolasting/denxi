#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Uninstalling Packages}

Uninstalling a package feels similar to installing a package,
in that the command accepts the same kind of arguments.

@verbatim|{
$ xiden uninstall def.rkt
$ xiden uninstall https://example.com/def.rkt
$ xiden uninstall example.com:widget:draft
}|

The difference is that @binary will match installations against the
information you provide. Be warned that @tech{package queries} may
match more than one installed package. Uninstalling
@litchar{example.com:widget} will target every edition, revision, and
output of the named package.  @tech{Package definitions} won't have
this effect because they correspond to queries that match only
themselves.

For safety, @tt{uninstall} refuses to remove packages referenced by
another package. It also does not actually delete files. It only marks
matching packages as garbage. If you later install any packages that
are already installed but marked as garbage, then they won't be
garbage anymore.

To delete all garbage packages and unreferenced files in your
@tech{workspace}, run the garbage collector. This will commit
all pending deletions in your workspace.

@verbatim|{
$ xiden gc
}|
