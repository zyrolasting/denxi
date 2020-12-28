#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Model}

@project-name creates symbolic links to directories. Those directories
represent dependencies.  @|project-name| implements a garbage
collector that deletes any directory it once created once it has no
known symbolic links.

The linked directories are built lazily, atomically, deterministically
in @tech{workspaces} using @tech{package definitions}.
