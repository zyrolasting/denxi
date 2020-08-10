#lang scribble/manual

@require[@for-label[racket/base racket/file "../zcpkg-settings.rkt"]
         "shared.rkt"]

@title[#:style '(toc)]{A Zero-Collection Package Management System}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

This is a guide for @|binary|, a @tech/reference{collection}-less
package manager for Racket.

@table-of-contents[]

@include-section{concepts.scrbl}
@include-section{adding-removing.scrbl}
@include-section{queries.scrbl}
@include-section{workspace.scrbl}
@include-section{creating-packages.scrbl}
@include-section{versioning.scrbl}
@include-section{config.scrbl}
@include-section{hosting-packages.scrbl}
@include-section{publishing-packages.scrbl}
@include-section{reproduction.scrbl}
