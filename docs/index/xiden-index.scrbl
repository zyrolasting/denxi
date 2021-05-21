#lang scribble/manual

@require["../shared.rkt"]

@title[#:style '(toc)]{Xiden: Functional Dependency Management in Racket}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

Xiden is a dependency manager, and a platform for distributing
software. Think of it as Racket's answer to Guix. Licensed under
GPLv3.


@section{How to Use this Documentation}

Xiden's documentation is indexed here.  Each other document links back
to this one for navigation purposes.

@hyperlink["https://github.com/zyrolasting/xiden"]{Xiden's README}
covers promotional notes from the author, along with the benefits a
user can expect when using Xiden.

@other-doc[xiden-guide] covers how to install Xiden, and the most
typical ways to use it. It does not spend much time on corner cases or
nuances. New users should read this first.

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
