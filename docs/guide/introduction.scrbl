#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Why use Xiden?}

Chances are you've used @tt{raco pkg}, otherwise you wouldn't
encounter this documentation. I used @tt{raco pkg} until I understood
how Racket package management was expected to work. You couldn't pin
versions, you couldn't release breaking changes elegantly, and each
package installation mutated the running Racket installation. Worse, I
found that conflicts were possible if two complete strangers have an
overlapping directory structure in their projects.

I wrote Xiden to get out of that situation, based on what I knew about
functional programming, dependency management in large-scale
commercial projects, and sensible software design. This isn't to say
alternatives are bad, but I expect those open to trying new ideas may
enjoy a few benefits.

@itemlist[
@item{Xiden treats your explicit consent as sacrosanct, and the documentation educates you on the implications of your decisions.}
@item{Xiden defaults to a zero-trust configuration, because the alternative is @hyperlink["http://www.ranum.com/security/computer_security/editorials/dumb/"]{a really bad idea}.}
@item{Xiden has checks everywhere, from server authentication and signature verification to safety limits on data transfers. You can even revoke trust in cryptographic hash functions if one is compromised.}
@item{Xiden does not mutate your Racket installation when working. @tt{raco pkg} does.}
@item{Xiden supports Windows. Guix and Nix do not.}
@item{Xiden, being a Racket program, enables you to write DSLs for custom software distribution.}
@item{Xiden can be configured to accept custom notations in its
command-line interface, and to use data from any source.}
]
