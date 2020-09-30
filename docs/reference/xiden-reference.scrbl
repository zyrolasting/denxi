#lang scribble/manual

@require[@for-label[racket/base]
         "../shared.rkt"]

@title[#:style '(toc)]{@project-name Reference}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

This is the API reference for @|project-name|. For a high-level
overview, read the @hyperlink["../xiden-guide/index.html"]{guide}.

@bold{Warning}: The implementation is unstable.

@table-of-contents[]
@include-section{model.scrbl}
@include-section{xiden.scrbl}
@include-section{workspace.scrbl}
@include-section{settings.scrbl}
@include-section{private.scrbl}
