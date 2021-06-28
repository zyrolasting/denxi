#lang scribble/manual

@require[@for-label[racket/base]
         "../shared.rkt"]

@title{Xiden Topics}

Xiden distributes software, and this document covers related topics in
no particular order.

The best way to use this document is to visit cited sections from the
context of other documents.

For all documentation, see @other-doc[xiden-index].

@table-of-contents[]

@include-section{integrity.scrbl}
@include-section{signature.scrbl}
@include-section{declare-racket-versions.scrbl}
@include-section{declare-os-support.scrbl}
@include-section{user-metadata.scrbl}
@include-section{monads.scrbl}
@include-section{abstract-concrete.scrbl}
@include-section{override-inputs.scrbl}
@include-section{securing-launch.scrbl}
@include-section{conflicts.scrbl}
@include-section{duplicate-outputs.scrbl}
@include-section{determinism.scrbl}
@include-section{fetch-command.scrbl}
@include-section{show-command.scrbl}
@include-section{queries.scrbl}
@include-section{launcher-security.scrbl}
