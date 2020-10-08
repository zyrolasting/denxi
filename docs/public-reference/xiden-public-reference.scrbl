#lang scribble/manual

@require[@for-label[racket/base]
         "../shared.rkt"]

@title[#:style '(toc)]{@|project-name|: Public API Reference}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

This is the public API reference for @|project-name|.

For information on private APIs, see  @other-doc['(lib "xiden/docs/private-reference/xiden-private-reference.scrbl")].
For a high-level overview of @project-name, see  @other-doc['(lib "xiden/docs/guide/xiden-guide.scrbl")].

@bold{Warning}: The public API is currently unstable.

@table-of-contents[]
@include-section{model.scrbl}
@include-section{xiden.scrbl}
@include-section{workspace.scrbl}
@include-section{settings.scrbl}
@include-section{message.scrbl}
@include-section{logged.scrbl}
@include-section{codec.scrbl}
@include-section{integrity.scrbl}
@include-section{input-info.scrbl}
@include-section{url.scrbl}
@include-section{string.scrbl}
