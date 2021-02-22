#lang scribble/manual

@require[@for-label[racket/base]
         "../shared.rkt"]

@title[#:style '(toc)]{Xiden: A Guide for Functional Dependency Management in Racket}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

This is a guide for Xiden, a functional dependency manager for Racket.

This guide is for anyone familiar with Racket who wishes to have more
options when distributing their own software, or for using third party
code. When you are finished reading, you'll understand how Xiden
works, you'll understand what it's strengths/weaknesses are, and
you'll be better equipped to understand the reference material (See @other-doc['(lib "xiden/docs/reference/xiden-reference.scrbl")]).

To track Xiden's progress as a project, see @secref{project}.

@table-of-contents[]

@include-section{introduction.scrbl}
@include-section{setup.scrbl}
@include-section{creating-packages.scrbl}
@include-section{workspace.scrbl}
@include-section{config.scrbl}
@include-section{cli.scrbl}
@include-section{queries.scrbl}
@include-section{catalog.scrbl}
@include-section{security.scrbl}
@include-section{project.scrbl}
