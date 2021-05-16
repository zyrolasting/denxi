#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{What is Xiden? Why use it?}

Xiden creates symbolic links and the directories they mention. Xiden
is useful because it does so in a reproducible way, under a lot of
safety checks and a default zero-trust configuration.

I inially wrote Xiden to transcend the limitations of @tt{raco
pkg}. In @tt{raco pkg}, you couldn't pin versions, you couldn't
release breaking changes elegantly, and each package installation
mutated the running Racket installation. Worse, I found that conflicts
were possible if two complete strangers have an overlapping directory
structure in their projects, and those conflicts were not easy to
reconcile.

Now, I am writing Xiden to fulfil a mission: To make a dependency
manager that adapts to any process. No matter what form of dependency
hell or human error you encounter in third-party software, Xiden's
goal is to solve it without changing anything more than a
configuration.

Those open to trying Xiden will enjoy a few benefits.

@itemlist[

@item{Xiden treats your explicit consent as sacrosanct, and the
documentation educates you on the implications of your decisions.}

@item{Xiden defaults to a zero-trust configuration, because the
alternative is
@hyperlink["http://www.ranum.com/security/computer_security/editorials/dumb/"]{a
really bad idea}.}

@item{Xiden has checks everywhere, from server authentication and
signature verification to safety limits on data transfers. You can
even revoke trust in cryptographic hash functions if one is
compromised. It is also not difficult to swap Xiden's verification
backends from (say) OpenSSL to GPG.}

@item{Xiden does not mutate your Racket installation when working.}

@item{Xiden is cross platform.}

@item{Xiden, being a Racket program, enables you to write DSLs for
custom software distribution.}

@item{Xiden can be configured to accept custom notations in its
command-line interface, and to use data from any source.}

@item{Xiden follows functional programming principles to such a degree
that an entire Xiden program can be stored in memory as a Racket
value.}

]
