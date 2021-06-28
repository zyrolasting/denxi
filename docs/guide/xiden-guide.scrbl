#lang scribble/manual

@require[@for-label[racket/base]
         "../shared.rkt"]

@title[#:style '(toc)]{Xiden Guide}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

Xiden distributes software, and this guide will show you how to use it.

You'll learn enough to use the rest of the documentation in
@other-doc[xiden-index].

The guide avoids excessive detail, but will cite elaborating documents
in context using square brackets (e.g. [@topic{integrity}]). These
citations will help you gain experience with the other shown documents.

@table-of-contents[]

@include-section{introduction.scrbl}
@include-section{setup.scrbl}
@include-section{creating-packages.scrbl}
@include-section{launchers.scrbl}
@include-section{outroduction.scrbl}
