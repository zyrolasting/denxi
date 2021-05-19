#lang scribble/manual

@require["../shared.rkt"]

@title[#:style '(toc)]{Xiden: Functional Dependency Management in Racket}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

Xiden is a functional (as in “functional programming”) dependency
manager for Racket.


@section{How to Use this Documentation}

Xiden's documentation is indexed here.  Each other document links back
to this one for navigation purposes.

@other-doc[xiden-guide] covers how to install Xiden, and the most
typical ways to use it. It does not spend much time on corner cases or
nuances. @bold{Read this first.}

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


@section{Why use Xiden?}

Xiden is a powerful alternative to @tt{raco pkg}, Guix, and Nix.
Xiden allows you to distribute software to others by custom rules, to
the point you may feasibly produce your own Steam or App Store by
throwing an interface in front of Xiden.  Xiden is also suitable for
use as a component of continuous integration systems and operating
systems.

End users ultimately control how Xiden works on their system, so they
can act as their own naming authority for all software found over the
network. Naming conflicts are simply cache hits for a cache the user
controls.

Xiden's built-in launcher defaults to a zero-trust configuration that
covers cryptographic hash functions, signature verification, server
authentication, and safety limits for data transfers. The trust model
may adapt to information security incidents because you can always
revoke consent for compromised cryptographic operations and grant
consent to a hardened approach.

It is possible to iterate using Xiden's zero trust configuration to
build an exact trust profile, then encode that configuration as a
custom launcher for those who prefer convenience over security.  You
can distribute these custom launchers with Xiden itself, so that you
can easily equip developers and non-developers with the tools that
they need.

Xiden includes localization facilities for translating its messages to
other languages, and performs stateful operations atomically. It is
robust, extensible, and built to help you handle every case of
dependency hell.

I built Xiden with my experience solving software distribution
problems for companies with thousands of employees of varying
technical skill, with infrastructure spread across multiple
continents. Xiden is licensed under the GPL out of respect for
end-user rights. Xiden is available under non-free licenses for a
nominal fee.
