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

Xiden distributes software, and this is its documentation.  Each
document links back to this one.

@other-doc[xiden-guide] covers how to install Xiden, and the most
typical ways to use it. It does not spend much time on corner cases or
nuances. New users should read this first.

@other-doc[xiden-white-paper] covers distribution problems and Xiden's
overall approach to them.

@other-doc[xiden-tutorials] is similar to the guide, but each section
covers a well-defined topic. Any problems or scenarios shown in a
tutorial have well-defined solutions.

@other-doc[xiden-practices] has sections that are similar to
tutorials, where problems come from human error or habits.  The
document focuses on practices because @italic{how} the programmer
approaches the problem is more important than @italic{what} the
programmer uses.

@other-doc[xiden-reference] is a formal definition of the API, and all
modules in the @litchar{xiden} collection.
