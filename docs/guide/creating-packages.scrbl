#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title[#:tag "new-pkg"]{Defining Packages}

@project-name builds software using @tech{package definitions}.
Here is a hypothetical package for working with URIs.

@racketmod[
xiden

(define package "uri")
(define provider "example.com")
(define description "An implementation of the IETF's RFC 3986")
(define tags '("networking" "www" "uri"))
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
between.

The discovery information near the top of the document should make
sense at first glance. The following queries would match this package
definition:

@itemlist[
@item{@litchar{example.com:uri:draft:21}}
@item{@litchar{example.com:uri:draft:21:21:ii}}
@item{@litchar{example.com:uri:draft:alpha}}
@item{@litchar{example.com:uri:draft:alpha:::lib}}
]

@section{Package Inputs}

@racketblock[
  (input "code.tar.gz"
    (sources "https://example.com/packages/uri/artifacts/alpha.tgz"
             "https://mirror.example.com/uri/alpha.tgz")
             (integrity 'sha384 (base64 "KxAqYG79sTcKi8yuH/YkdKE+O9oiBsXIlwWs3pBwv/mXT9/jGuK0yqcwmjM/nNLe")))]

A package @deftech{input} is a request for exact bytes. In plain
language, this expression tells @project-name to make a file called
@racket{code.tar.gz} available in our directory when
building. @project-name will try to fetch the archive from the given
@racketfont{sources}, and will raise an error if it cannot match the
integrity information.


@subsection{Authenticating Inputs}

You can declare a signature with an input. A signature expression
expects an asymmetric cipher algorithm, a expression of the
signature's bytes, and an expression of the public key used to verify
the signature.

This example uses a base64-encoding of a DES signature, with a
public-key fingerprint.

@racketblock[(input (sources "https://example.com/path/to/artifact")
                    (integrity sha384 (base64 "KxAqYG79sTcKi8yuH/YkdKE+O9oiBsXIlwWs3pBwv/mXT9/jGuK0yqcwmjM/nNLe"))
                    (signature des (base64 "") (fingerprint "")))]

@binary uses a signature and a public key you (presumably) trust to
confirm that the @italic{digest} was signed with someone's private
key.

@subsection{About Trusting Public Keys}

The presence of a signature has @italic{nothing} to do with how much
you should trust the input.

In general, your trust in an input depends on if the public key you
use to verify the signature actually came from a party you trust.
Vetting public keys is out of scope for this guide.

If you are unsure about the validity of a signature, then declaring
that signature will offer no added protection.

@subsection{Using Signatures in a Self-Contained Definition}

Remember that @binary understands the signature as expressions of
bytes. While it would be painful to read, you can embed the raw byte
content of each piece of data.

@racketblock[(input (sources "https://example.com/path/to/artifact")
                    (integrity sha384 (base64 "KxAqYG79sTcKi8yuH/YkdKE+O9oiBsXIlwWs3pBwv/mXT9/jGuK0yqcwmjM/nNLe"))
                    (signature des (base64 "") #"... a lot of bytes ..."))]

Why do this? Because it makes the package definition self-contained.
That means you do not need to configure @binary to look up more
information to understand a package definition.

If you cannot stand how the package definition looks with a full
public key pasted in the file, then read @secref{merging}.


@section{Package Outputs}

Package outputs are a little easier to grasp. They are just names like
@racket{doc}, @racket{lib}, or @racket{gui}. They list accepted
arguments to the build procedure.

Package outputs do not declare integrity information. Since a
package's output can serve as another package's input, the bits would
be verified once they are used as input.


@subsection{What About Nondeterministic Builds?}

If a package's output contains changing data like an embedded
timestamp, then the digest of that output will change. That prevents
you from using the same integrity information to verify a package's
output. It does not make sense to respond by leaving out integrity
information, because that level of trust is a security vulnerability.

In practice, this is only a problem if an input's source returns
different information across builds. If that happens, then that is an
issue to take up with whoever owns that source. Package inputs are
also not expected to be things like bytecode files, they are expected
to be source code or other inputs to a Racket program acting
@italic{as} a build system.



@section{Declare Supported Racket Versions}

@racket[racket-versions] is a list of pairs, where each pair is an inclusive
interval of Racket versions you support for this package. Gaps in versions
are not expected, but you can express them for flexibility.

If @binary is running in a Racket installation that does not match
@racket[racket-versions], it will raise an error. It can, however, be
forced to install the package anyway.
