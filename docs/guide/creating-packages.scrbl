#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title[#:tag "new-pkg"]{Defining a Package}

@project-name builds software using @tech{package definitions}. In this section
we will write our own step by step. Alternatively, you can skip to
@secref{finished-definition}.

@section{Create a New Definition File}

Create a new blank Racket module. The name of that file is up to you, but I'll
use @tt{definition.rkt}. We will use the @racketmodname[xiden] language.

@racketmod[#:file "definition.rkt" xiden]

Being a Racket programmer, you probably used the @racketmodname[info] language
to define your Racket packages. Good news: You already know most of the
@racketmodname[xiden] language too. Every valid @racketmodname[info] module is
also a valid @racketmodname[xiden] module.


@section{The Usual Stuff}

Every dependency management tool has you write the same things, so let's start
with them. You can define the name of your package, your identity as a provider,
a short description of your package, tags, and so on. Everything in this example
should not need much explanation.

Since I'm hosting the files for this guide on my site, I'll use my information
here.

@racketmod[#:file "definition.rkt"
xiden

(define package "my-first-package")
(define provider "sagegerard.com")
(define description "Fun playtime in a tutorial")
(define tags '("fun" "tutorial" "example"))
(define home-page "https://sagegerard.com")
]

@racket[provider] is a little less obvious. A provider is not necessarily the
author of the package, but rather the name of the party responsible for
distributing the software defined in this file. In this case, they are the same
party. There's no restriction on how you name yourself as a provider, but a
domain name is useful as an identifier across hosts.


@section[#:tag "versioning"]{Declare the Version}

Next, let's declare the version of our package.

@racketmod[#:file "definition.rkt"
xiden

(define package "my-first-package")
(define provider "sagegerard.com")
(define description "Fun playtime in a tutorial")
(define tags '("fun" "tutorial" "example"))
(define home-page "https://sagegerard.com")

(define edition "default")
(define revision-number 0)
(define revision-names '("alpha"))
]

But wait, nothing there looks like a version number. @project-name versions
@tech{package definitions} using @tech{editions} and @tech{revisions}, not
major or minor version numbers. This means users can find software like they
would a book. When defining a package, you may specify an @tech{edition}, a
@tech{revision number}, and any @tech{revision names} to act as aliases for
that number.


@subsection{Editions}

An @deftech{edition} is a name for a design or target audience. Think
of it as a semantic alternative to a major version number. When you
wish to adapt your software to a different audience without disrupting
existing users, change the edition.

The default name of an edition is @racket{default}.

Editions address technical and social concerns. They allow you to segment your
user base without changing the (branded) name of your software or the general
value it offers. This may be preferable to forcing one implementation to
accomodate everyone. You can use this as an excuse to refactor, rewrite, or
delete swaths of code that are irrelevant to an audience.  It also helps one
avoid nonsensical numerical schemes that try to track breaking changes across
all possible audiences.

Define editions sparingly and thoughtfully to keep maintenance costs
manageable.


@subsection{Revisions}

A @deftech{revision} is a numbered implementation of an @tech{edition}. Given
an edition, a user can select a @tech{package definition} using a
@tech{revision number} or a @tech{revision name}.


@subsubsection{Revision Numbers}

A @deftech{revision number} is a non-negative integer.

A @tech{package definition} @italic{must} include a @tech{revision number}.  If
you are releasing a new package with the same @tech{edition}, then increment
the @tech{revision number}. If you are starting a new edition, then the
@tech{revision number} must start at @racket[0] as it does in the examples on
this page.


@subsubsection{Revision Names}

A @deftech{revision name} is an alias for a @tech{revision number}. A
@tech{revision name} can be any string that contains at least one non-digit.

A @tech{package definition} may include a list of at least zero @tech{revision
names} for the @tech{revision number}. A @tech{revision name} should be unique
within an @tech{edition}.  If a user searches for a package definition using a
@tech{revision name}, and that name refers to more than one @tech{revision
number} in an @tech{edition}, then this should be seen as a mistake on the
provider's part.


@subsection{Opinion: What About Semantic Versioning?}

@hyperlink["https://semver.org/"]{Semantic Versioning} (“SemVer”)
allows you to infer what happened to software depending on which of
three version numbers changed. Any assumptions about the change hold
so long as the relevant package author follows SemVer rules closely
and voluntarily. Since SemVer depends on ideal human compliance like
every other scheme, I don't trust it unconditionally.

In my mind, a version is a form of identification. Jeff Atwood used
the term “dog tag,” since we often ask a user what version they are
using when our program fails out in the field. This makes sense, but
versions can also help users find what they want. Not even technical
users will know off-hand if version 3.49.111 is a good fit for them,
but most of them @italic{do} understand the latest revision of a
teacher's edition. Anyone knowledgeable enough to use software is
likely able to discriminate between two editions or revisions of the
same book. I don't see why software should be any harder to
evaluate. If I later extended this idea to a versioning scheme that
not only identified software, but captured the nature of all related
changes, then I would only tell you long after consulting a patent
attorney.

We can do without the bureaucracy and social pressure of rules like
those defined in SemVer. The scheme defined on this page offers no
pretense that you can look at two versions and know if the most recent
one is backwards-compatible. All this scheme does is give you room to
divide up your userbase into audiences and your product into designs.
Then, you can chronicle releases in terms of your relationship with
each audience. This is messy, but that's life. At least with this
scheme you can decide what changes to make in terms of the people
affected by those changes. That's as good as it gets.

If you still prefer Semantic Versioning after reading all that, then
you can always define an @tech{edition} that uses semantic version
strings as @tech{revision names} and use a plugin that resolves
Semantic Version queries. See @secref{plugin} for more information.


@section{Declare Supported Racket Versions}

Next, we should decide what versions of Racket we want to support when building
@italic{and running} our software. For that, we define @racket[racket-versions]
as a list of pairs, where each pair is an inclusive interval of Racket versions
you support for this package.

This example, defines software that can run from Racket v6.0 to Racket
v7.7.0.5.

@racketblock[
(define racket-versions '(("6.0" . "7.7.0.5")))
]


If @racket[(version)] is not an element of the set defined by
@racket[racket-versions], @project-name will raise an error.

Gaps in versions are not expected due to Racket's commitment to
backwards-compatibility, but you can express them in the event one version
behaves strangely for you.

You can also declare version support as unbounded on one side of an interval
using @racket[#f]. This definition of @racket[racket-versions] matches every
version of Racket except those strictly between 7.2 and 7.4.

@racketblock[
(define racket-versions '((#f . "7.2") ("7.4" . #f)))
]


All that being said, we want to handle the common case where we assume that we
will support as many Racket versions as we can and stay backwards compatible.
Here we'll define support for v5.0 and up.

@racketmod[#:file "definition.rkt"
xiden

(define package "my-first-package")
(define provider "sagegerard.com")
(define description "Fun playtime in a tutorial")
(define tags '("fun" "tutorial" "example"))
(define home-page "https://sagegerard.com")

(define edition "default")
(define revision-number 0)
(define revision-names '("alpha"))

(define racket-versions '(("5.0" . #f)))
]


@section{Package Inputs}

Now we're getting into the interesting stuff. In @|project-name|, a package is
a program. Programs have inputs, so your dependencies are viewed as inputs to a
package. Specifically, a @deftech{package input} is a deferred request for
exact bytes. I'll just define one for now.


@racketmod[#:file "definition.rkt"
xiden

(define package "my-first-package")
(define provider "sagegerard.com")
(define description "Fun playtime in a tutorial")
(define tags '("fun" "tutorial" "example"))
(define home-page "https://sagegerard.com")

(define edition "default")
(define revision-number 0)
(define revision-names '("alpha"))

(define racket-versions '(("5.0" . #f)))

(define inputs
  (list (input "default.tgz"
               (sources "https://sagegerard.com/xiden-tutorial/default.tgz"))))]


This input defines an archive of source code we'll need to build our project.
It contains a throwaway Racket module and Scribble document.
@racket{default.tgz} is the name of the file that we use locally in our
build. The @racket[sources] list tells @project-name where it can find the
actual bytes for that file. I'm only using one source here, but you can add
mirrors or relative paths to check.


@subsection{Everything is an Input}

A @tech{package input} can be any file, not just Racket packages or code.
You can use a Python source archive as an input, or a critical security patch.
This means that you can use @project-name to coordinate cross-ecosystem builds.

While we won't cover it here, another benefit of package inputs is that you can
substitute them. If a build is taking too long because compiles a huge project
from source, you can adjust the definition to use pre-built binaries instead.


@subsection{Integrity Information}

Okay, so we named a file that we want. But how do we know we got the right file?
For that, we need to declare integrity information with our input.

@racketblock[
(define inputs
  (list (input "default.tgz"
               (sources "https://sagegerard.com/xiden-tutorial/default.tgz"))
               (integrity 'sha384 (hex "299e3eb744725387e0355937727cf6e3c938eda2355cda82d58596fd535188fa624217f52f8c6e7d5ee7cb1d458a7f75"))))]

The integrity information tells @project-name if it got the @italic{right}
bytes. If @project-name cannot get the @italic{exact bytes} this input demands,
then the build will fail. This is a good thing! It makes builds reproducible,
so long as the build produces the same output from the same input. An input
might only be available during a build, or may persist after a build for
run-time use. More on that later.

If you are not familiar with integrity checking, just know that there are
functions to take a file and turn it into a fixed-length byte string called a
@italic{digest}.  If two files produce the same digest, then we can assume the
files are the same. That is, unless the function itself has a
@italic{collision}, where two different files produce the same digest. This is
a sign to use a different function!

The function we're using in this case is SHA-384, which we represent here as
@racket['sha384]. Since it's hard to type the exact bytes of a digest, we can
give @project-name the expected digest as a string we copy and paste from
elsewhere.  Here we tell @project-name that the SHA-384 digest of
@racket{default.tgz} comes from a hex string. That tells @project-name how to
translate the digest as a string back to bytes for comparison.

If you followed @secref{setup}, then you should have OpenSSL installed on
your system. You can check the digest for yourself by downloading the
file at the link shown in the code, and then running this command:

@verbatim|{
$ openssl dgst -sha384 default.tgz
SHA384(default.tgz)= 299e3eb744725387e...
}|

There's a tricky part here. Yes, the digests match our code, but that doesn't
mean you should paste it right into new definitions. Typically you want to
write a definition using a @italic{trusted copy} of a file, such as one you
keep on your drive or from a repository you maintain.


@subsection{Authenticating Inputs}

@margin-note{This is the hardest part of the tutorial, but stick with it. It
will help you stay safe when working with resources from the Internet.  You
need a working understanding of how to use OpenSSL to check digital signatures
to understand this section.}

It's one thing to get the bytes we wanted, but did they come from someone we
trust? Thankfully it is possible for people to sign their work so that we can
check.

You may declare a signature with an input. A signature expression includes a
source of a public key used to verify the signature, and a source for the
signature itself. The public key, when stored as a file called
@litchar{public.pem} must work when used in @litchar{openssl pkeyutl -pubin
-inkey public.pem ...}.

Signatures are applied per-input. This example fetches both a public key and a
signature from the same host that provides an artifact. Here I use a public key
that is only used for this tutorial.

@racketblock[
(define inputs
  (list (input "default.tgz"
               (sources "https://sagegerard.com/xiden-tutorial/default.tgz"))
               (integrity 'sha384 (hex "299e3eb744725387e0355937727cf6e3c938eda2355cda82d58596fd535188fa624217f52f8c6e7d5ee7cb1d458a7f75"))
               (signature "https://sagegerard.com/xiden-tutorial/public.pem"
                          "https://sagegerard.com/xiden-tutorial/default.tgz.sign")))]

The @racket[signature] form accepts a string that locates a public key, and a
string that locates a signature, in that order. @binary uses the signature and
public key to confirm that the @italic{raw bytes of the digest} specified in
the @racket[integrity] information was signed with a corresponding private key.

While @project-name can fetch public keys from the Internet for you, it will
refuse to process any input where you do not affirm your trust in the
corresponding public key.  Vetting public keys is out of scope for this
guide. Just know that if you do not trust the public key, then a signature
verified by that key won't offer you any value.  See @secref{trusting-pubkeys}
to learn how to affirm trust for individual public keys.


@section{Package Outputs}

The processing step occurs in the @deftech{package build procedure}, which
creates files to fulfill a requested @deftech{package output}. A @tech{package
output} is a human-readable name for a possible deliverable from the package,
such as documentation, libraries, or tests.

@racketblock[
(define (build target)
  (unpack (input-ref inputs (string-append target ".tgz"))))]

In this example, @racket[target] is bound to the requested @tech{package output}.
But, we haven't defined any outputs in our @tech{package definition}.  That's
okay. Every package definition has an implicit @racket{default} output, even if
@racket[outputs] is not defined. If a user does not request a particular output
from a package, then @project-name will use the @racket{default} output.

Recall in the last section that we defined an input named @racket{default.tgz}.
This means that the build will fetch and extract that archive.

The @tech{package build procedure} runs in a sandbox (as in
@racketmodname[racket/sandbox]) to mitigate the damage caused by malicious
code. For added safety, @|project-name|'s own OS-level permissions should be
limited.  When building, @racket[current-directory] is bound to a unique
directory based on the definition itself. This makes it such that two packages
can only conflict if evidence overwhelmingly points to those packages being
identical.  This means that you can assume the directory is empty, and yours to
populate.


@subsection{Adding a New Output}

Assume @racket{default.tgz} has everything a user would need. Some users might
only want the libraries, not the documentation. Storage is cheap, but hundreds
of dependencies add up, you know?

We can define a new output for our budget-conscious users:

@racketblock[
(define outputs '("minimal"))
]

This says that we have a minimal output available, but notice that I did not
define @racket{default}. I could, but it would be redundant. @racket{default}
is always defined.

By doing this, we actually changed what arguments can be passed to a
@tech{package build procedure}. Previously, @project-name would only ever bind
@racket[target] to @racket{default}. Now it can bind it to @racket{default}, or
@racket{minimal}.

Since the build extracts an archive with the same name as the output, We'll
need a new @tech{package input} to go with this output.

@racketblock[
(define inputs
  (list (input "default.tgz"
               (sources "https://sagegerard.com/xiden-tutorial/default.tgz")
               (integrity 'sha384 (hex "299e3eb744725387e0355937727cf6e3c938eda2355cda82d58596fd535188fa624217f52f8c6e7d5ee7cb1d458a7f75"))
               (signature "https://sagegerard.com/xiden-tutorial/public.pem"
                          "https://sagegerard.com/xiden-tutorial/default.tgz.sign"))
        (input "minimal.tgz"
               (sources "https://sagegerard.com/xiden-tutorial/minimal.tgz")
               (integrity 'sha384 (hex "6cc38a7e2513fa9abd2ac079e9c8efbab9385458275c927e77527a189ed9ac393d734a4cf306787425bf722a5ac025c6"))
               (signature "https://sagegerard.com/xiden-tutorial/public.pem"
                          "https://sagegerard.com/xiden-tutorial/minimal.tgz.sign"))))]

Now when our users choose to build @racket{minimal} output, they will only ever
download and extract the @racket{minimal.tgz} archive.


@subsection{Equivalent Outputs Cause Duplicate Data}

@project-name assumes that different outputs produce different content, which
makes a human error possible. Assume we changed @racket[build] such that
@racket{default} was used as an alias of another output.

@racketblock[
(define outputs '("full" "minimal"))

(define (build target)
  (unpack (input-ref inputs
                     (string-append (if (equal? target "default")
                                        "full"
                                        "minimal")
                                    ".tgz"))))]

This works, but if a user requests the @racket{full} output and then the
@racket{default} output, then the same archive would be extracted twice into
different directories.  This pollutes the disk with redundant data, which is
probably not what you want.

Again, different @tech{package outputs} are expected to produce different
things. If you believe that two outputs are equivalent, then combine them into
one output.

@section[#:tag "finished-definition"]{The Finished Definition}

Here is the file we've authored. To recap, it defines a build that simply
extracts an archive depending on the requested output. We'll discuss this
definition further in @secref{cli}.

@racketmod[#:file "definition.rkt"
xiden

(define package "my-first-package")
(define provider "example.com")
(define description "Fun playtime in a tutorial")
(define tags '("fun" "tutorial" "example"))
(define home-page "https://sagegerard.com")

(define edition "default")
(define revision-number 0)
(define revision-names '("alpha"))

(define racket-versions '(("5.0" . #f)))

(define inputs
  (list (input "default.tgz"
               (sources "https://sagegerard.com/xiden-tutorial/default.tgz")
               (integrity 'sha384 (hex "299e3eb744725387e0355937727cf6e3c938eda2355cda82d58596fd535188fa624217f52f8c6e7d5ee7cb1d458a7f75"))
               (signature "https://sagegerard.com/xiden-tutorial/public.pem"
                          "https://sagegerard.com/xiden-tutorial/default.tgz.sign"))
        (input "minimal.tgz"
               (sources "https://sagegerard.com/xiden-tutorial/minimal.tgz")
               (integrity 'sha384 (hex "6cc38a7e2513fa9abd2ac079e9c8efbab9385458275c927e77527a189ed9ac393d734a4cf306787425bf722a5ac025c6"))
               (signature "https://sagegerard.com/xiden-tutorial/public.pem"
                          "https://sagegerard.com/xiden-tutorial/minimal.tgz.sign"))))

(define outputs '("default" "minimal"))

(define (build target)
  (untgz (input-ref inputs (string-append target ".tgz"))))
]
