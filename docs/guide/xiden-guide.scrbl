#lang scribble/manual

@require[@for-label[racket/base]
         "../shared.rkt"]

@title[#:style '(toc)]{Xiden Guide}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

This is a guide for Xiden, a functional (as in “functional
programming”) dependency manager for Racket.

For all documentation, see @other-doc[xiden-index].

Xiden creates directories atomically with your explicit and informed
consent. Xiden is also deterministic, robust, configurable, and
extensible. It does not have the limitations of @tt{raco pkg}, and is
suitable for commercial software distribution.

In this guide you will learn how to release and install software using
Xiden. When you are finished, you'll understand how Xiden works well
enough to use the rest of the documentation.

@table-of-contents[]

@include-section{introduction.scrbl}
@include-section{setup.scrbl}
@include-section{creating-packages.scrbl}
@include-section{launchers.scrbl}
@include-section{queries.scrbl}
@include-section{cache.scrbl}
@include-section{project.scrbl}
