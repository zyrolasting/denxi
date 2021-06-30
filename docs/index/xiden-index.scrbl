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

@other-doc[xiden-guide] introduces Xiden.

@other-doc[xiden-reference] defines Xiden's API.

@other-doc[xiden-white-paper] explains the thinking behind Xiden.
