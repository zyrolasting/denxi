#lang scribble/manual

@require[@for-label[racket/base]
         "../shared.rkt"]

@title[#:style '(toc)]{Xiden Reference}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

This is the API reference for Xiden.

For all documentation, see @other-doc[xiden-index].

@bold{Warning}: The API is currently unstable.

@table-of-contents[]
@include-section{cli.scrbl}
@include-section{model.scrbl}
@include-section{support.scrbl}
@include-section{maintenance.scrbl}
