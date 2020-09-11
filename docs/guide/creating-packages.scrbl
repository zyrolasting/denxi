#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title[#:tag "new-pkg"]{Defining Packages}

@project-name builds software using @tech{package definitions}. Here
is a hypothetical definition for a URI package.

@racketmod[
xiden

(define package "uri")
(define provider "example.com")
(define description "An implementation of the IETF's RFC 3986")
(define tags '("networking" "www" "uri" "rfc3986"))
(define home-page "https://example.com/packages/uri")
(define edition "draft")
(define revision-number 21)
(define revision-names '("alpha"))
(define racket-versions '(("6.0" . "7.7.0.5")))

(define source-code
  (input "code.tar.gz"
    (sources "https://example.com/packages/uri/artifacts/alpha.tgz"
             "https://mirror.example.com/uri/alpha.tgz")
             (integrity 'sha384 (base64 "KxAqYG79sTcKi8yuH/YkdKE+O9oiBsXIlwWs3pBwv/mXT9/jGuK0yqcwmjM/nNLe"))))

(define minimal-source-code
  (input "code-minimal.tar.gz"
    (sources "https://example.com/packages/uri/artifacts/alpha.tgz"
             "https://mirror.example.com/uri/alpha.tgz")
             (integrity 'sha384 (base64 "KxAqYG79sTcKi8yuH/YkdKE+O9oiBsXIlwWs3pBwv/mXT9/jGuK0yqcwmjM/nNLe"))))


(define inputs (list minimal-source-code source-code))

(define outputs '("lib" "doc" "all"))

(define (build target)
  (unpack (string-append target ".tar.gz")))
]


A definition consists of inputs, outputs, and a build procedure in
between. Most of the discovery information near the top of the
document should make sense at first glance. We'll spend more time on
the less typical definitions.


@section{Versioning}

@project-name versions @tech{package definitions} as if they were
published documents, not software. This means reasoning about change
in terms of @tech{editions} and @tech{revisions}, not major or minor
version numbers.


@racketblock[
(define edition "draft")
(define revision-number 21)
(define revision-names '("alpha"))
]

An @deftech{edition} is @bold{a name for a design or target
audience}. It acts as a semantic alternative to a major version
number. When you wish to adapt your software to a different audience
without disrupting existing users, change the edition.

A @deftech{revision} is an @bold{implementation of a design for a
given target audience}. It can be a @tech{revision number} or a
@tech{revision name}. There is no guarentee that every revision is
backwards-compatible, but since the audience is presumed to be the
same, how welcome a breaking change would be depends on the
relationship between the maintainers and the target audience.

A @deftech{revision number} is a non-negative integer. Every revision
is forever assigned the next available number in an edition.

A @deftech{revision name} is an alias for a @tech{revision number}. It
can be any string that contains at least one non-digit.

A package must have a @tech{revision number}. When changed, a package must
increment its @tech{revision number} if the change uses the same
@tech{edition}. If the package starts a new edition, the @tech{revision number}
must reset to @racket[0].

The default name of an edition is @racket{default}. By the above
rules, every package starts on the zeroth revision of the
@racket{default} edition.


@section{Package Inputs}

A package @deftech{input} is a deferred request for exact bytes.

@racketblock[
  (input "code.tar.gz"
    (sources "https://example.com/packages/uri/artifacts/alpha.tgz"
             "https://mirror.example.com/uri/alpha.tgz")
             (integrity 'sha384 (base64 "KxAqYG79sTcKi8yuH/YkdKE+O9oiBsXIlwWs3pBwv/mXT9/jGuK0yqcwmjM/nNLe")))]

In plain language, this expression tells @project-name that a build
will need @racket{code.tar.gz} available. @project-name will lazily
fetch the archive from the given @racketfont{sources}.

An input might only be available during a build, or may persist after
the build complete for run-time use.  The only difference from
@|project-name|'s is whether the input is subject to garbage
collection after a build completes.


@subsection{Authenticating Inputs}

You can declare a signature with an input. A signature expression
expects an asymmetric cipher algorithm, a expression of the
signature's bytes, and an expression of the public key used to verify
the signature.

This example uses a base64-encoding of a DES signature, with a
public-key fingerprint.

@racketblock[(input (sources "https://example.com/path/to/artifact")
                    (integrity 'sha384 (base64 "KxAqYG79sTcKi8yuH/YkdKE+O9oiBsXIlwWs3pBwv/mXT9/jGuK0yqcwmjM/nNLe"))
                    (signature 'des (base64 "") (fingerprint "")))]

@binary uses a signature and a public key you (presumably) trust to
confirm that the @italic{digest} was signed with someone's private
key. Vetting public keys is out of scope for this guide. Just know
that if you do not trust the public key, then a signature offers no
added protection.


@subsection{Merging Definitions}

You can combine package definitions together to create new
definitions. If you have one package definition that is hard to read
because of the embedded integrity information, you can instead write
an abbreviated version.

@racketmod[
xiden

(define package "uri")
(define provider "example.com")
(define edition "draft")
(define revision-number 21)
(define revision-names '("alpha"))
(define racket-versions '(("6.0" . #f)))

(define inputs
  (list (input "lib.tar.gz")
        (input "doc.tar.gz")
        (input "all.tar.gz")))

(define outputs
  '("lib" "doc" "all"))

(define (build target)
  (unpack (input-ref (string-append target ".tar.gz"))))
]

After doing so, you can then merge the definitions.  Leverage this to
override inputs, remove outputs, or migrate definitions. Merging from
different sources means you can leverage community mods for
established packages.


@verbatim|{
$ xiden merge light.rkt example.com:calculator https://gist.githubusercontent.com/... > modded.rkt
}|


@section{Package Outputs}

@racketblock[
(define outputs
  '("lib" "doc" "all"))
]

Package outputs are a little easier to grasp. They are just names like
@racket{doc}, @racket{lib}, or @racket{gui}. They list accepted
arguments to the build procedure.

Package outputs do not declare integrity information. Since a
package's output can serve as another package's input, the bits would
be verified once they are used as input.

Every package definition has an implicit @racket{default} output, even
if @racket[outputs] is not defined. If a user does not request a
particular output from a package, then @project-name will use the
@racket{default} output.


@subsection{On Nondeterministic Builds}

If a package's output contains changing data like an embedded
timestamp, then the digest of that output will change. That prevents
you from using the same integrity information to verify a package's
output. It does not make sense to respond by leaving out integrity
information, because that level of trust is a security vulnerability.



@section{Declare Supported Racket Versions}

@racket[racket-versions] is a list of pairs, where each pair is an inclusive
interval of Racket versions you support for this package. Gaps in versions
are not expected, but you can express them for flexibility.

If @binary is running in a Racket installation that does not match
@racket[racket-versions], it will raise an error. It can, however, be
forced to install the package anyway.
