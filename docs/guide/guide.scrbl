#lang scribble/manual

@require[@for-label[racket/base]
         "../shared.rkt"]

@title[#:style '(toc)]{@|project-name|: A Functional Dependency Manager}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

This is a guide for @|project-name|, a functional dependency manager
for Racket.

@binary safely and deterministically reproduces your project's
dependencies without any side-effect on the running Racket
installation. This makes it a powerful and versatile way to share work
and continuously integrate code.

@bold{@project-name is not yet production ready.} Dependency
management is a hard problem that takes time to do well. To track
@|project-name|'s progress, please visit the source code at GitHub
below.

@hyperlink["https://github.com/zyrolasting/xiden"]{https://github.com/zyrolasting/xiden}

@table-of-contents[]

@include-section{concepts.scrbl}
@include-section{setup.scrbl}
@include-section{creating-packages.scrbl}
@include-section{managing-dependencies.scrbl}
@include-section{versioning.scrbl}
@include-section{queries.scrbl}
@include-section{workspace.scrbl}
@include-section{config.scrbl}
@include-section{plugin.scrbl}
