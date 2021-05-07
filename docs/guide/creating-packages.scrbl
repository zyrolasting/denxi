#lang scribble/manual

@require["../shared.rkt" @for-label[@except-in[xiden/pkgdef #%module-begin] racket/base]]

@title[#:tag "new-pkg"]{Packages}

Xiden defines a @deftech{package} as an active instance of a program
called a @deftech{package definition}. This will seem odd to those who
think of packages as inert artifacts. After all, we don't normally
“execute” packages. In Xiden, package @bold{definitions} are the inert
artifacts. The difference between a package definition and a package
is the difference between a program or a process.

In this section we will write a @tech{package definition} using the
@racketmodname[xiden] language. We'll use this definition to create
and install a package in @secref{launchers}.

The @racketmodname[xiden] language keeps just enough of Racket to be
useful for what we want to do. We can define variables, write
procedures, and operate on basic data types. But the language forbids
@racket[require], I/O, and any terms that would cause side-effects on
instantiation.

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
            (integrity 'sha384 (src filename ".dgst"))
            (signature (src "public.pem")
                       (src filename ".sign"))))
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

The best URL to put in @racket[url] is one that offers clarifying
information for the package itself, hence why it points to this page.


@section[#:tag "versioning"]{Declare the Version}

Package definitions versions have an @tech{edition} and a
@tech{revision}.

An @deftech{edition} is a name for a target audience. When you wish to
adapt your software to a different audience without disrupting
existing users, define a new edition.

A @deftech{revision} is an implementation of an @tech{edition}. Given
an edition, a user can select a @tech{package definition} using a
@tech{revision number} or a @tech{revision name}.

A @deftech{revision number} is an exact non-negative integer. If you
are releasing a new @tech{package definition} with the same edition,
then increment the revision number. If you are starting a new edition,
then the revision number is @racket[0].

A @deftech{revision name} is a string that contains at least one
non-digit. Each revision name is a unique alias for a revision number
within an edition. A package definition may include a list of at least
zero revision names for the revision number.


@section{Declare Inputs and Outputs}

A package definition is a program. Like any program, it has inputs and
outputs. A @deftech{package input} is a named datum that won't take up
space until it is actually used. A @deftech{package output} is a named
subprogram that builds files using inputs. When a user installs a
package with Xiden, they select an output from that package.

Writing inputs can be tedious, so we define procedures to capture the
patterns among them. I expressed two inputs using @deftech{artifacts}.
Aritfacts are a way to express a source of bytes along with the means
to verify that we got the right bytes from the right party.

Skilled readers will notice that this definition tries to deliver an
artifact and the information used to verify it from the same
potentially untrusted, non-deterministic source.  Don't be alarmed:
Xiden won't trust it either, unless the user consents. We'll show how
this plays out later in the guide.

The package outputs shown here are simple, but some might be taken
aback by the fact they require a functional programming background to
extend. Here's a different way to write the same @racket{minimal}
output.

@racketblock[
(output "minimal"
        archive-input := (input-ref "minimal-local.tgz")
        archive-path  := (resolve-input archive-input)
        (extract-input archive-path))]

This notation is a way to write programs using monadic types. If you
are familiar with Haskell and its @tt{do} notation, then you'll feel
at home. If you are not sure what I mean, see @secref[#:doc
xiden-tutorials]{monads}.
