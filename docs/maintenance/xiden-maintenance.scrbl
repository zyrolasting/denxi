#lang scribble/manual

@require["../shared.rkt"]

@title{@|project-name|: Maintainer's Reference}

This document covers development and maintenance information pertinent
to @|project-name|. This document will not try to repeat what the code
says, but will instead offer broader context behind why things are the
implemented the way they are. That is, the context that cannot be
adequately summarized in code comments.

If you were looking for the API reference, see @other-doc['(lib
"xiden/docs/reference/xiden-reference.scrbl")]. If you
want a high-level overview of what @project-name is, see
@other-doc['(lib "xiden/docs/guide/xiden-guide.scrbl")].

@table-of-contents[]

@include-section{basics.scrbl}
@include-section{affirmations.scrbl}
