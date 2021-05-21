#lang scribble/manual

@require["../shared.rkt"
         @for-label[@except-in[xiden/pkgdef #%module-begin]
                    xiden/integrity
                    xiden/signature
                    racket/base]]

@title[#:tag "new-pkg"]{Packages}

We don't normally “execute” packages, but you do in Xiden. In Xiden, a
@deftech{package definition} is a program used to create packages and
install one of their outputs. The difference between a package
definition and a package is the difference between a program and a
process.

In this section we will write a @tech{package definition} using the
@racketmodname[xiden] language. We'll use this definition to create
and install a package in @secref{launchers}.

The @racketmodname[xiden] language allows you to define variables,
write procedures, and operate on basic data types. But the language
forbids @racket[require], I/O, and any side-effects on instantiation.
@bold{Think of a package definition as the declarative form of a
software release.}

@racketmod[#:file "definition.rkt"
xiden

(code:comment "Declare Discovery information")
(name "my-first-package")
(provider "sagegerard.com")
(description "Fun playtime in a tutorial")
(tags "fun" "tutorial" "example")
(url "https://docs.racket-lang.org/xiden-guide/new-pkg.html")

(code:comment "Declare the Version")
(edition "default")
(revision-number 0)
(revision-names "alpha" "pre-release-0")

(code:comment "Declare Inputs and Outputs")
(input "default-local.tgz"
       (archive-artifact "default.tgz"))

(input "minimal-local.tgz"
       (archive-artifact "minimal.tgz"))

(output "default" (extract-input "default-local.tgz"))
(output "minimal" (extract-input "minimal-local.tgz"))

(code:comment "Utility Procedures")
(define (src . parts)
  (http-source
    (apply string-append
           "https://sagegerard.com/xiden-tutorial/"
           parts)))

(define (archive-artifact filename)
  (artifact (src filename)
            (integrity 'sha384 (src filename ".sha384"))
            (signature (src "public.pem")
                       (src filename ".sha384.sign"))))
]



@section{Declare Discovery Information}

You can define the name of your package, your identity as a provider,
a short description of your package, tags, and so on. Those items
should not need much explanation. The @racket[provider] and
@racket[url] definitions are less obvious.

A provider is not necessarily the author of the package, but rather a
name of the party responsible for distribution. In this case, they are
the same party.

There's no restriction on how you name a provider, but a domain name
is useful as a verifiable identifier when data is split across
different networks.

The @racket[url] term is meant to help users find clarifying
information for the package itself, hence why it points to this page.
If you do not have a page for your package, then you may safely omit
@racket[url].


@section[#:tag "versioning"]{Declare the Version}

Package definitions have versions, and each version consists of an
@tech{edition} and a @tech{revision}.

An @deftech{edition} is a string that names a target audience, or a
design that caters to that audience. When you wish to adapt your
software to a different audience without disrupting existing users,
define a new edition.

A @deftech{revision} is an implementation of an @tech{edition}.  We'll
assume that a user would prefer the latest revision of their chosen
edition, but user can select a revision using a @tech{revision number}
or a @tech{revision name}.

A @deftech{revision number} is an exact non-negative integer. If you
are releasing a new @tech{package definition} with the same edition,
then increment the revision number. If you are starting a new edition,
then the revision number is @racket[0].

A @deftech{revision name} is a string that contains at least one
non-digit. Each revision name is a unique alias for a revision number
within an edition. A package definition may include a list of at least
zero revision names for the revision number.


@section{Declare Inputs and Outputs}

As we saw, a package definition is a program. Like any program, we can
define inputs and outputs. A @deftech{package input} is a named source
of data which may or may not be available in the package definition. A
@deftech{package output} is a named subprogram that builds files using
package inputs.

When a user installs software using Xiden, they are required to select
a package output. Only the inputs actually used by that output are
saved to disk.

Writing inputs can be tedious, so we can define procedures to capture
some patterns. I expressed two inputs using @deftech{artifacts}, which
express the source of data for an input along with the means to verify
that we got the right data from the right party. For more information,
read @secref[#:doc xiden-tutorials "integrity"], and then
@secref[#:doc xiden-tutorials "signature"].

Skilled readers will see a problem by now.  The artifact and the
information used for verification are coming from the same potentially
untrusted source.  Don't be alarmed: Xiden does not volunteer trust in
this situation.  @practice{determinism} covers how to adjust
definitions for the sake of reproducible builds, after the user
consents to this arrangement.

The package outputs shown here are simple, but some might be taken
aback by the fact they require a working understanding of monads. If
you are not sure what I mean, please read @secref[#:doc
xiden-tutorials]{monads} before proceeding any further.

Here's a different way to write the same @racket{minimal} output.

@racketblock[
(output "minimal"
        archive-input := (input-ref "minimal-local.tgz")
        archive-path  := (resolve-input archive-input)
        (extract-input archive-path))]

This notation is a way to write programs using monadic types. Haskell
programmers will understand the notation without issue.

@section[#:tag "error-openssl-intinfo"]{Troubleshooting OpenSSL for Integrity Information}

On this page we use SHA-384 for integrity information, but your
OpenSSL build might not include it. If you get an error like
@litchar{subprocess timeout} later, run @litchar{openssl list
-digest-commands} to see what options are available to you.

This tutorial's integrity information is available in @litchar{md5},
@litchar{sha1}, @litchar{sha256}, @litchar{sha384}, and
@litchar{sha3-384}. For example, if you want to use @litchar{sha1},
update your definition to replace the hash function like so:

@racketblock[
(define (archive-artifact filename)
  (artifact (src filename)
            (integrity 'sha1 (src filename ".sha1"))
            (signature (src "public.pem")
                       (src filename ".sha1.sign"))))
]

@bold{Be sure to use an option that is both hosted on the server, and
shown in @litchar{openssl list -digest-commands}.}
