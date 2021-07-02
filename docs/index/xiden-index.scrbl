#lang scribble/manual

@require[racket/format
         racket/runtime-path
         scribble/core
         scribble/html-properties
         "../shared.rkt"]

@title[#:style '(toc)]{Xiden Documentation}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

@(define logo-element
   (elem #:style
    (style #f
           (list (alt-tag "img")
                 (attributes
                  `((src . "https://github.com/zyrolasting/xiden/raw/master/docs/index/doc-logo.png")
                    (style . "max-width: 100%")))))))

@logo-element

@centered{
Xiden distributes software, and this is its documentation.

@other-doc[xiden-guide] - @other-doc[xiden-reference] - @other-doc[xiden-white-paper]
}
