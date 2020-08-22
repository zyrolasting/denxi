#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title[#:tag "new-pkg"]{Defining Packages}

@binary creates packages from @deftech{package definitions}. The below
example defines a hypothetical package for working with URIs. The
package expects the source code and a setup program as input. Once
that input is available, it will produce libraries, documentation, and
tests in terms of the input setup programs.

@racketmod[
info

(define racket-versions '(("6.0" . "7.7.0.5")))

(define inputs
  '((input "code.tar.gz"
           (sources "https://example.com/packages/uri/artifacts/alpha.tgz"
                    "https://mirror.example.com/uri/alpha.tgz")
           (integrity sha384 (base64 "KxAqYG79sTcKi8yuH/YkdKE+O9oiBsXIlwWs3pBwv/mXT9/jGuK0yqcwmjM/nNLe")))
    (input "setup.rkt"
           (sources "https://example.com/packages/uri/artifacts/setup.rkt"
                    "https://mirror.example.com/uri/setup.rkt")
           (integrity sha384 (base64 "IlknabsnNFuFTTuDOKSjdHUio2qBVXC82y6Z6kzWVlVzNW4p2wL/ldLiC5FgVFWk")))))

(define outputs
  '[(output "lib"  ("racket" "setup.rkt" "lib"))
    (output "doc"  ("racket" "setup.rkt" "full"))
    (output "test" ("racket" "setup.rkt" "full"))])
]

@section{A Package Definition is not Coupled to Source Code}

Notice that the source code is declared as an input. This means
that source code does not have to be present when defining
this package.

If you are accustomed to working with @tt{raco pkg}, then you leave
@tt{info.rkt} files in your source code as if they were assembly
instructions for @tt{raco pkg} and @tt{raco setup} to follow. That's
not the case here. @binary views package definitions as input to
generate a whole other program that expects @italic{exact bits}
as input for some process.


@section{Determinism or Bust}

@racketblock[(... (integrity sha384 (base64 "KxAqYG79sTcKi8yuH/YkdKE+O9oiBsXIlwWs3pBwv/mXT9/jGuK0yqcwmjM/nNLe")))]

Package inputs are declared with integrity information. @binary
supports various cryptographic hash algorithms and various encodings
of digests. For the sake of safety and reproducible builds,
inputs should be declared with integrity information.

When @binary fetches an input, it will fail if the bytes do not
produce the same hash with the same algorithm.

If working with integrity information is problematic, then you can shut off
@|binary|'s integrity check. @bold{Please don't, that's dreadfully unsafe}.

You should also avoid SHA-1 and MD5, because their digests can be
spoofed. Aim for SHA-384 from the SHA-2 suite as a baseline.


@section{Where's the Output Integrity Information?}

Astute readers would have already noticed that package outputs do not
declare integrity information. A package's output can serve as another
package's input, then the bits would be verified at that time.


@section{What About Nondeterministic Builds?}

If a package's output contains changing data like an embedded timestamp,
then the digest of that output will change. That prevents you from using
the same integrity information to verify a package's output as a whole.
It does not make sense to simply leave out integrity information
because that can be an attack vector.

In practice, this is only a problem if an input's source returns
different information across builds. If that happens, then that
is an issue to take up with whoever owns that source.


@section{Authenticating Inputs}

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


@section{Shipping Code Differently}

There are no mandated conventions for sharing work. If an author
chooses to bundle their source code in their setup module, they can do
this:

@racketblock[
(define inputs
  '((input "setup.rkt"
           (sources "https://example.com/packages/uri/artifacts/setup-combined.rkt"
                    "https://mirror.example.com/uri/setup-combined.rkt")
           (integrity sha384 (base32 "kKX/nRNdAmfJ77GPTs5XYF/DOESsGB97V54GUQuia+zYTQ86MaHqtk3tkgGuO3Tt")))))
]

Notice that the @racket[outputs] field does not have to change,
because the Racket module used to install the package is assigned the
same name @racket{setup.rkt} in @racket[inputs]. The package discovery
information does not have to change unless the implementation behaves
differently.

All that changes in this example is the actual content of the input,
which impacts where the script comes from and its digest.


@section{Declare Supported Racket Versions}

@racket[racket-versions] is a list of pairs, where each pair is an inclusive
interval of Racket versions you support for this package. Gaps in versions
are not expected, but you can express them for flexibility.

If @binary is running in a Racket installation that does not match
@racket[racket-versions], it will raise an error. It can, however, be
forced to install the package anyway.


@section{Discovering a Package}

So far we've discussed the parts of a @tech{package definition} that
@binary actually uses. It would help if we added human-readable
information to help others understand what a package is for.

@racketmod[
info

(define package "uri")
(define description "An implementation of the IETF's RFC 3986")
(define tags '("networking" "www" "uri"))
(define home-page "https://example.com/packages/uri")
(define edition "draft")
(define revision-number 21)
(define revision-names '("alpha"))
(define provider "example.com")

(code:comment "[...]")
]

Note that this information does not impact how @binary works.
You can add any data you'd like to a definition.

These fields should make sense at a glance: They exist solely to
inform humans or machines about the package. The only aspect that
might not be familiar is how the package is versioned.  @binary does
not actually care about how you version your software, but it does
ship with a plugin that matches @tech{package queries} to package
definitions.

The @tech{query} @tt{example.com:uri:draft:21} matches the above
package definition. You can see that a query consists of the
@racket[provider], @racket[package], @racket[edition], and
@racket[revision-number] fields.

If you set @racket[revision-names] to @racket['("initial" "oldest"
"beginning")], then users can replace the @racket[0] in their query
with any of those strings and still get this @tech{package
definition}. Choose revision names wisely: Hosts might not be able to
make sense of a query if you reuse revision names within an edition.
