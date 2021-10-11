#lang scribble/manual

@require[racket/format
         racket/runtime-path
         scribble/core
         scribble/html-properties
         "../shared.rkt"]

@title[#:style '(toc)]{Denxi Documentation}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

@(define logo-element
   (elem #:style
    (style #f
           (list (alt-tag "img")
                 (attributes
                  `((src . "https://github.com/zyrolasting/denxi/raw/master/docs/index/doc-logo.png")
                    (style . "max-width: 100%")))))))

@logo-element

@centered{
Denxi distributes software, and this is its documentation.

@other-doc[denxi-guide] - @other-doc[denxi-reference] - @other-doc[denxi-white-paper]
}