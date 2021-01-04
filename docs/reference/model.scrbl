#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Model}

Xiden creates symbolic links to directories. Those directories
represent dependencies.  Xiden implements a garbage
collector that deletes any directory it created once it has no known
symbolic links. Users therefore uninstall software by deleting links
and running a garbage collection pass.

The linked directories are built lazily, atomically, and
deterministically in @tech{workspaces} using @tech{package
definitions}.
