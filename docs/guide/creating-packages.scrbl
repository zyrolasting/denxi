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


(define inputs
  (list (input "default.tgz"
               (sources "https://example.com/packages/uri/artifacts/alpha.tgz"
                        "https://mirror.example.com/uri/alpha.tgz")
                        (integrity 'sha384 (base64 "KxAqYG79sTcKi8yuH/YkdKE+O9oiBsXIlwWs3pBwv/mXT9/jGuK0yqcwmjM/nNLe")))
        (input "minimal.tgz"
               (sources "https://example.com/packages/uri/artifacts/alpha.tgz"
                        "https://mirror.example.com/uri/alpha.tgz")
                        (integrity 'sha384 (base64 "KxAqYG79sTcKi8yuH/YkdKE+O9oiBsXIlwWs3pBwv/mXT9/jGuK0yqcwmjM/nNLe")))))


(define outputs '("minimal"))

(define (build target)
  (unpack (input-ref inputs (string-append target ".tgz"))))
]


A definition consists of inputs, outputs, and a build procedure in
between. Most of the discovery information near the top of the
document should make sense at first glance.


@section{Package Versions}

@project-name versions @tech{package definitions} using
@tech{editions} and @tech{revisions}, not major or minor version
numbers. This means users can discover and select packages much like
they would select a book or paper. When defining a package, you may
specify an @tech{edition}, a @tech{revision number}, and any
@tech{revision names} to act as aliases for that number.

@racketblock[
(define edition "draft")
(define revision-number 21)
(define revision-names '("alpha"))
]

See @secref{versioning} for information about this versioning scheme.


@section{Package Inputs}

A package @deftech{input} is a deferred request for exact bytes.

@racketblock[
(input "code.tar.gz"
       (sources "https://example.com/[...]"
                "https://mirror.example.com/[...]")
       (integrity 'sha384 (base64 "O9oiBsXIlwWs3pBwv[...]")))]

When building a package, @project-name will lazily produce the named files in
the build directory when a build tries to use those files.  If it cannot
produce bits that match the given digest, then the build will fail.

An input might only be available during a build, or may persist after
a build for run-time use.  The only difference from @|project-name|'s
perspective is whether the input is subject to garbage collection
after a build completes. @secref{managing} shows that any file
without a reference is eligible for collection by @litchar{xiden gc}.
The same applies for any input used in a build.


@section{Package Outputs}

@deftech{Package outputs} are human-readable names for possible deliverables
from the package.

Package outputs do not declare integrity information. Since a package's output
can serve as another package's input, any bits would be verified once they are
used as input.

Every package definition has an implicit @racket{default} output, even if
@racket[outputs] is not defined. If a user does not request a particular output
from a package, then @project-name will use the @racket{default} output.

The example at the beginning of this page shows @racket[(define outputs
'("minimal"))].  This means that @project-name can apply the build procedure to
the strings @racket{minimal} or @racket{default}.

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


@section{Equivalent Outputs Cause Duplicate Data}

@project-name assumes that different outputs produce different content. A
@tech{package definition} might not handle this properly.

Consider this build procedure, which unpacks either @racket{default.tgz} or
@racket{minimal.tgz}.

@racketblock[
(define outputs '("minimal"))

(define (build target)
  (unpack (input-ref inputs (string-append target ".tgz"))))]

This works without problems, but what if we changed @racket[build] such that
@racket{default} was used as an alias of another output?

@racketblock[
(define outputs '("full" "minimal"))

(define (build target)
  (unpack (input-ref inputs
                     (string-append (if (equal? target "default")
                                        "full"
                                        "minimal")
                                    ".tgz"))))]

This is fine, but it changes the domain of the @racket[build] procedure to
accept @racket{full}, @racket{minimal}, and @racket{default}. If a user
were to request the @racket{full} output and the @racket{default} output,
then the same archive would be extracted twice into different directories.
This pollutes the disk with redundant data, which is probably not what
you want.

The @racket{default} output is meant to be used as a unique output name in its
own right, not as an alias.


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

@italic{This section requires a working understanding of OpenSSL and how one
verifies a signature using a public key.}

@project-name supports authenticating inputs using OpenSSL and trusted public
keys.

You may declare a signature with an input. A signature expression includes a
source of the public key used to verify the signature, and a source for the
signature itself. Since this is done per-input, it allows trusted teammates to
each sign the artifact they are responsible for maintaining. If you are an
independent developer, then you may simply express your same public key for
each input.

This example fetches both a public key and a signature from the
same host that provides an artifact.

@racketblock[(input (sources "https://example.com/path/to/artifact.tar.gz")
                    (integrity 'sha384 (base64 "KxAqYG79sTcKi8yuH/YkdKE+O9oiBsXIlwWs3pBwv/mXT9/jGuK0yqcwmjM/nNLe"))
                    (signature "https://example.com/public.pem"
                               "https://example.com/path/to/artifact.sign"))]

@binary uses a signature and a public key to confirm that the @italic{digest}
specified in the @racket[integrity] information was signed with a corresponding
private key.

While @project-name can fetch public keys from the Internet for you, it will
refuse to process any input where you do not affirm your trust in the
corresponding public key.  Vetting public keys is out of scope for this
guide. Just know that if you do not trust the public key, then a signature
verified by that key won't offer you any value.  See @secref{trusting-pubkeys}
to learn how to affirm trust for individual public keys.

There are some caveats:

@itemlist[

@item{Not every cipher algorithm supports every digest. It is your
responsibility to use a digest supported by the cipher algorithm
backing your keys.}

@item{@project-name assumes that signatures are derived from the @italic{raw
bytes} of the digest. This means that if you create a signature from
an ANS.1-encoded digest using a command like @litchar{openssl dsgt
-sign ...}, then @project-name will see a signature mismatch.}

@item{If you want to use a @tech{package definition} as an input, and no
signature for the definition is published, then you have to make a judgement
call. Using an unsigned @tech{package definition} means trusting @italic{every}
unsigned input behind that definition.}

]
