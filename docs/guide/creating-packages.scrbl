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

(define outputs '("lib" "doc"))

(define (build target)
  (unpack
   (input-ref
    (string-append (if (equal? target "lib") "code-minimal" "code")
                   ".tgz"))))
]


A definition consists of inputs, outputs, and a build procedure in
between. Most of the discovery information near the top of the
document should make sense at first glance.


@section{Versioning}

@project-name versions @tech{package definitions} using
@tech{editions} and @tech{revisions}, not major or minor version
numbers.

@racketblock[
(define edition "draft")
(define revision-number 21)
(define revision-names '("alpha"))
]

An @deftech{edition} is @bold{a name for a design or target
audience}. It acts as a semantic major version. When you wish to adapt
your software to a different audience without disrupting existing
users, change the edition.

A @deftech{revision} is an @bold{implementation of a design for a
given target audience}. It can be a @tech{revision number} or a
@tech{revision name}. There is no guarentee that every revision is
backwards-compatible, but since the audience is presumed to be the
same, how welcome a breaking change would be depends on the
relationship between the maintainers and the target audience.
Any automated upgrades should be designed around this.

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


(define inputs (list minimal-source-code source-code))]

In plain language, this expression tells @project-name that a build
will need @racket{code.tar.gz} available. @project-name will lazily
fetch the archive from the given @racketfont{sources}.

An input might only be available during a build, or may persist after
a build for run-time use.  The only difference from @|project-name|'s
perspective is whether the input is subject to garbage collection
after a build completes.


@section{Package Outputs}

@racketblock[
(define outputs '("lib" "doc"))]

@deftech{Package outputs} are just strings that humans understand as
names for possible deliverables from the package. Every package
definition has an implicit @racket{default} output, even if
@racket[outputs] is not defined. If a user does not request a
particular output from a package, then @project-name will use the
@racket{default} output.

Package outputs do not declare integrity information. Since a
package's output can serve as another package's input, the bits would
be verified once they are used as input.


@section{Package Processing}

Let's go back to freshman year: There's inputs, there's outputs, and
then there's processing.  The processing step occurs in the
@deftech{package build procedure}.

@racketblock[
(define (build target)
  (unpack
   (input-ref
    (string-append (if (equal? target "lib") "code-minimal" "code")
                   ".tgz"))))
]

A @tech{package build procedure} creates output files from
inputs. @project-name will only ever bind @racket[target] to one of
the strings defined in @racket[outputs], or @racket{default}. You may
use bindings from the @racketmodname[xiden] language to lazily fetch
inputs and prepare files on a user's system.

This build procedure only unpacks an archive. It also illustrates how
an output name can translate to an input name. The @racket[input-ref]
procedure accepts a string name you defined for the input and finds
the related entry in the defined @racket[inputs] list.

Due to Racket's scoping rules, you can use or even define inputs
directly in the build procedure.

@racketblock[
(define (build target)
  (unpack (if (equal? target "lib") minimal-source-code source-code)))]

That shortens the code, so why not do this all the time? Because then
@project-name and analysis tools will have a harder time understanding
the package definition.

The build procedure runs in a sandbox (as in
@racketmodname[racket/sandbox]) to mitigate the damage caused by
malicious code. For added safety, @|project-name|'s own OS-level
permissions should be limited.  When building,
@racket[current-directory] is bound to a unique directory. The name of
that directory is a cryptographic hash based on inputs and relevant
names. This makes it such that two packages can only conflict if
evidence overwhelmingly points to those packages being identical.
This means that you can assume the directory is empty, and yours to
populate.


@section{Declare Supported Racket Versions}

@racketblock[
(define racket-versions '(("6.0" . "7.7.0.5")))
]

@racket[racket-versions] is a list of pairs, where each pair is an
inclusive interval of Racket versions you support for this
package. The example defines a package that can run from Racket v6.0
to Racket v7.7.0.5.

Gaps in versions are not expected due to Racket's commitment
to backwards-compatibility, but you can express them in the event
one version behaves strangely for you.

If @racket[(version)] is not an element of the set defined by
@racket[racket-versions], @project-name will raise an error.

You can declare version support as unbounded on one side of an
interval using @racket[#f]. This definition of
@racket[racket-versions] matches every version of Racket except those
strictly between 7.2 and 7.4.

@racketblock[
(define racket-versions '((#f . "7.2") ("7.4" . #f)))
]


@section{Authenticating Inputs}

@italic{This feature is incomplete, but you can review how it works at
a high-level here.}

You can declare a signature with an input. A signature expression
expects an asymmetric cipher algorithm, a expression of the
signature's bytes, and an expression of the public key used to verify
the signature.

This example uses a base64-encoding of a DES signature, with a
public-key fingerprint.

@racketblock[(input (sources "https://example.com/path/to/artifact")
                    (integrity 'sha384 (base64 "KxAqYG79sTcKi8yuH/YkdKE+O9oiBsXIlwWs3pBwv/mXT9/jGuK0yqcwmjM/nNLe"))
                    (signature 'des (base64 "...") (fingerprint "...")))]

@binary uses a signature and a public key you (presumably) trust to
confirm that the @italic{digest} was signed with someone's private
key. Vetting public keys is out of scope for this guide. Just know
that if you do not trust the public key, then a signature offers no
added protection.
