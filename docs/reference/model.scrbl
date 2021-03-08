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

This section covers the modules that cooperate to these ends, under
various safety mechanisms and customizations.

@table-of-contents[]
@include-section{modules/pkgdef.scrbl}
@include-section{modules/package.scrbl}
@include-section{modules/main.scrbl}
@include-section{modules/version.scrbl}
@include-section{modules/query.scrbl}
@include-section{modules/workspace.scrbl}
@include-section{modules/source.scrbl}
@include-section{modules/catalog.scrbl}
@include-section{modules/input-info.scrbl}
@include-section{modules/plugin.scrbl}
@include-section{settings.scrbl}
@include-section{verification.scrbl}
