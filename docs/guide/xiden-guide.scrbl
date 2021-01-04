#lang scribble/manual

@require[@for-label[racket/base]
         "../shared.rkt"]

@title[#:style '(toc)]{Xiden: A Guide for Functional Dependency Management in Racket}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

This is a guide for Xiden, a functional dependency manager
for Racket.

Xiden safely and deterministically reproduces your project's
dependencies without any side-effect on the running Racket installation. This
makes it a powerful and versatile way to share work and continuously integrate
code.

To track Xiden's progress, see @secref{project}.

If you need the reference material, see  @other-doc['(lib "xiden/docs/reference/xiden-reference.scrbl")].

@table-of-contents[]

@include-section{introduction.scrbl}
@include-section{setup.scrbl}
@include-section{creating-packages.scrbl}
@include-section{workspace.scrbl}
@include-section{config.scrbl}
@include-section{cli.scrbl}
@include-section{queries.scrbl}
@include-section{security.scrbl}
@include-section{project.scrbl}
