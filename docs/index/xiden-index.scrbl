#lang scribble/manual

@require[racket/format
         racket/runtime-path
         scribble/core
         scribble/html-properties
         "../shared.rkt"]

@title[#:style '(toc)]{Xiden Documentation}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

@define-runtime-path[logo]{doc-logo.png}

@(define logo-element
   (elem #:style
    (style #f
           (list (alt-tag "img")
                 (attributes
                  `((src . "./doc-logo.png")
                    (style . "max-width: 100%")))))))

@logo-element

Xiden distributes software, and this is its documentation.

@other-doc[xiden-guide] covers how to install and use Xiden.

@other-doc[xiden-white-paper] covers Xiden's approach to distribution
problems.

@other-doc[xiden-tutorials] covers how to perform well-defined tasks.

@other-doc[xiden-practices] covers problems without clear-cut
solutions.

@other-doc[xiden-reference] covers a formal API definition.
