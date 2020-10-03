#lang scribble/manual

@require[@for-label[racket/base]
         "../shared.rkt"]

@title[#:style '(toc)]{Xiden: A Pure Dependency Manager}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

This is a guide for @|project-name|, a functional dependency manager
for Racket.

@project-name safely and deterministically reproduces your project's
dependencies without any side-effect on the running Racket installation. This
makes it a powerful and versatile way to share work and continuously integrate
code.

To track @|project-name|'s progress, see @secref{project}.

If you need the reference material, see  @other-doc['(lib "xiden/docs/public-reference/xiden-public-reference.scrbl")].

@table-of-contents[]

@include-section{concepts.scrbl}
@include-section{setup.scrbl}
@include-section{creating-packages.scrbl}
@include-section{managing-dependencies.scrbl}
@include-section{versioning.scrbl}
@include-section{queries.scrbl}
@include-section{workspace.scrbl}
@include-section{config.scrbl}
@include-section{trust.scrbl}
@include-section{plugin.scrbl}
@include-section{project.scrbl}
