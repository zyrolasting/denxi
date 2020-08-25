#lang scribble/manual

@require[@for-label[racket/base]
         "../shared.rkt"]

@title[#:style '(toc)]{A Functional Package Management System}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

This is a guide for @|binary|, a functional package manager for Racket.

@binary concerns itself with the safe, deterministic reproduction of
exact dependencies without any side-effect on the running Racket
installation.

@table-of-contents[]

@include-section{concepts.scrbl}
@include-section{adding.scrbl}
@include-section{removing.scrbl}
@include-section{queries.scrbl}
@include-section{workspace.scrbl}
@include-section{creating-packages.scrbl}
@include-section{versioning.scrbl}
@include-section{config.scrbl}
@include-section{plugin.scrbl}
@include-section{reproduction.scrbl}
