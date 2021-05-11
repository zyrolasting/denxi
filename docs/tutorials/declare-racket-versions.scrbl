#lang scribble/manual

@require[@for-label[racket
                    @except-in[xiden/pkgdef #%module-begin]]]

@title[#:tag "racket-versions"]{Limit Packages to Specific Racket Versions}

Write the @racket[racket-versions] form in a package definition to
declare what versions of a Racket installation may use your
package. When you run a package, Xiden will check if the running
version of Racket is an element of the set defined by
@racket[racket-versions]. If it isn't, that halts use of a package.

Be careful with how you understand this feature, because there's an
important nuance. Remember that Xiden can install arbitrary software,
so most package definitions won't even include
@racket[racket-versions]. It's only used when a package means to
install Racket code for use with the same Racket installation running
Xiden at the time. If Xiden is running on Racket v7.9, and it is about
to install a package that claims to only work on v8.0 and up, then
Xiden can reject the package. Do not take this to mean that Xiden can
only install Racket code, or that package definition itself won't load
for certain Racket versions.

This example @racket[racket-versions] form restricts the package for
use from Racket v6.0 to Racket v7.7.0.5. Each list of two versions is
an inclusive interval, so support includes the shown versions.

@racketblock[
(racket-versions ("6.0" "7.7.0.5"))
]

Gaps in versions are not expected due to Racket's commitment to
backward compatibility, but you can express them in the event one
Racket version does not interact well with your release.

You can also declare version support as unbounded on one side of an
interval using @racket{*}. This definition of @racket[racket-versions]
matches every version of Racket except those @italic{strictly between}
@racket{7.2} and @racket{7.4}.

@racketblock[
(racket-versions ("*" "7.2") ("7.4" "*"))
]

If you have particular behavior that depends on exact Racket
versions, then you may call out those individual versions. This
example adds such a version that would otherwise be excluded.

@racketblock[
(racket-versions ("*" "7.2") ("7.4" "*") "7.0.1.2")
]

It is likely you'll want to support as many Racket versions as
possible while staying backward compatible.  You can define support
for v5.0 and up using a definition like this.

@racketmod[#:file "definition.rkt"
xiden

(name "my-first-package")
(provider "example.com")
(description "I'm meant for Racket 5.0+")
(revision-number 0)

(racket-versions ("5.0" "*"))
]
