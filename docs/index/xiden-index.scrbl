#lang scribble/manual

@require["../shared.rkt"]

@title[#:style '(toc)]{Xiden: Functional Dependency Management in Racket}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

Xiden is a functional (as in “functional programming”) dependency
manager for Racket. Xiden is a powerful alternative to @tt{raco pkg},
Guix, and Nix. You can use Xiden to not only download software, but to
distribute it to others by custom rules. Xiden's flexible approach
makes it suitable for use as a component of continuous integration
systems and operating systems.

If you wish to try Xiden, start by reading @other-doc[xiden-guide].

If you have already adopted Xiden, you can read
@other-doc[xiden-tutorials] to learn how to complete common,
well-defined tasks. Read @other-doc[xiden-practices] to learn
tradeoffs behind different approaches to more nebulous problems.
Finally, you can read @other-doc[xiden-reference] for a formal
definition of the API.
