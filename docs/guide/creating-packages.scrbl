#lang scribble/manual

@require["../shared.rkt" @for-label[@except-in[xiden/pkgdef do #%module-begin] racket/base]]

@title[#:tag "new-pkg"]{Defining Packages}

@project-name builds software using @tech{package definitions}. In
this section we will write a package definition that, when installed,
extracts one of two available archives.

We will cover why we can feel confident in the build due to the
integrity checking and authentication steps that happen before we use
the archives.

If you are already familiar with how package definitions
work and just want an example to copy, then skip to
@secref{finished-definition}.


@section{Create a New Definition File}

Create a new blank Racket module. The name of that file is up to you, but I'll
use @tt{definition.rkt}. We will use the @racketmodname[xiden] language.

@racketmod[#:file "definition.rkt" xiden]

@section{The Usual Stuff}

Every dependency management tool has you write the same things, so
let's start there. Since I'm hosting some files for this guide on my
site, I'll use related information.

You can define the name of your package, your identity as a provider,
a short description of your package, tags, and so on. Everything in
this example should not need much explanation.

@racketmod[#:file "definition.rkt"
xiden

(package "my-first-package")
(provider "sagegerard.com")
(description "Fun playtime in a tutorial")
(tags "fun" "tutorial" "example")
(url "https://sagegerard.com")
]

The @racket[provider] definition is less obvious. A provider is not
necessarily the author of the package, but rather a name of the party
responsible for distribution. In this case, they are the same
party.

There's no restriction on how you name a provider, but a domain name
is useful as a verifiable identifier when data is split across
different networks.


@section[#:tag "versioning"]{Declare the Version}

Next, let's declare the version of our package.

@racketmod[#:file "definition.rkt"
xiden

(package "my-first-package")
(provider "sagegerard.com")
(description "Fun playtime in a tutorial")
(tags "fun" "tutorial" "example")
(url "https://sagegerard.com")

(edition "default")
(revision-number 0)
(revision-names "alpha" "2020-10-01")
]

But wait, nothing there looks like a version number. @project-name
versions @tech{package definitions} using @tech{editions} and
@tech{revisions}, not major or minor version numbers. This means users
can find software like they would a book. When defining a package, you
may specify an @tech{edition}, a @tech{revision number}, and any
@tech{revision names} to act as aliases for that number. The revision
names are freeform, but should relate to meaningful, unique stages in
your project's life.



@subsection{Editions}

An @deftech{edition} is a name for a design or target audience. Think
of it as a semantic alternative to a major version number. When you
wish to adapt your software to a different audience without disrupting
existing users, change the edition.

Editions address technical and social concerns. They allow you to
divide up your user base without changing the (branded) name of your
software or the general value it offers. This may be preferable to
forcing one implementation to accomodate everyone. You can use this as
an excuse to refactor, rewrite, or delete swaths of code that are
irrelevant to an audience.  It also helps one avoid nonsensical
numerical schemes that try to track breaking changes across all
possible audiences.

Define editions sparingly and thoughtfully to keep maintenance costs
manageable.


@subsection{Revisions}

A @deftech{revision} is a numbered implementation of an @tech{edition}. Given
an edition, a user can select a @tech{package definition} using a
@tech{revision number} or a @tech{revision name}.


@subsubsection{Revision Numbers}

A @deftech{revision number} is an exact non-negative integer.

If you are releasing a new @tech{package definition} with the same
@tech{edition}, then increment the @tech{revision number}. If you are
starting a new edition, then the @tech{revision number} must start at
@racket[0] as it does in the examples on this page.


@subsubsection{Revision Names}

A @deftech{revision name} is an alias for a @tech{revision number}. A
@tech{revision name} can be any string that contains at least one non-digit.

A @tech{package definition} may include a list of at least zero @tech{revision
names} for the @tech{revision number}.

@bold{A @tech{revision name} should be unique within an
@tech{edition}}.  If a user searches for a package definition using a
@tech{revision name}, and that name refers to more than one
@tech{revision number} in an @tech{edition}, then the provider is
responsible for correcting the ambiguity.


@subsection{Optional Reading: What About Semantic Versioning?}

@hyperlink["https://semver.org/"]{Semantic Versioning} (“SemVer”)
allows you to infer what happened to software depending on which of
three version numbers changed. Any assumptions about the change hold
so long as the relevant package author follows SemVer rules closely
and voluntarily. Since SemVer depends on ideal human compliance like
every other scheme, I don't think it contributes much in the end.

In my mind, a version is a form of identification. Jeff Atwood used
the term “dog tag,” since we often ask a user what version they are
using when our program fails out in the field. This came before the
simple advice to just use a date or timestamp for each version.

This makes sense, but neither scheme helps users with discovery. No
one knows if version 3.49.111 or Oct-10-2017 is a good fit for them
without additional context. But even non-technical users would
understand that they probably want a specific revision of a teacher's
edition. I don't see why software should be any harder to evaluate
than a book. If I later extended this idea to a versioning scheme that
not only identified software, but captured the nature of all related
changes, then I @italic{might} tell you after consulting a patent
attorney.

We can do without the bureaucracy and social pressure of rules like
those defined in SemVer. The scheme defined on this page offers no
pretense that you can look at two versions and know if the most recent
one is backwards-compatible. All this scheme does is give you room to
divide up your userbase into audiences and your product into designs.
Then, you can chronicle releases in terms of @italic{your relationship
with each audience}. This can be messy, but that's life. At least with
this scheme you can decide what changes to make in terms of the people
affected by those changes. That's as good as it gets.

If you still prefer Semantic Versioning after reading all that, then
you can always define an @tech{edition} that uses semantic version
strings as @tech{revision names} and use a plugin that resolves
Semantic Version queries. See @secref["Plugins" #:doc '(lib
"xiden/docs/reference/xiden-reference.scrbl")] for more information.


@section{Declare Supported Racket Versions}

Next, we can decide what versions of Racket we want to support if our
package includes a Racket program. For that, we use
@racket[racket-versions]. When you run a package, @project-name will
check if the running version of Racket is an element of the set
defined by @racket[racket-versions]. If it isn't, that halts use
of a package.

This example defines software that can run from Racket v6.0 to Racket
v7.7.0.5. Each list of two versions is an inclusive interval, so
support includes the shown versions.

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

We likely want to support as many Racket versions as we can while
staying backward compatible.  Let's just define support for v5.0 and
up.

@racketmod[#:file "definition.rkt"
xiden

(package "my-first-package")
(provider "sagegerard.com")
(description "Fun playtime in a tutorial")
(tags "fun" "tutorial" "example")
(url "https://sagegerard.com")

(edition "default")
(revision-number 0)
(revision-names "alpha")

(racket-versions ("5.0" "*"))
]


@section{Declare Supported Operating Systems}

Racket is cross-platform, but your package might not be.  Maybe you
need PowerShell. I won't judge. Limiting OS support allows you to make
reasonable assumptions about available binaries (e.g. GNU coreutils),
and for offering tailored experiences to users.

You can declare the operating systems you support by writing
@racketid[os-support] with a list of acceptable values of
@racket[(system-type 'os)].

This example is a statement of support for UNIX-like systems,
Windows, and MacOSX.

@racketblock[
(os-support unix windows macosx)
]

By default, @project-name assumes each package definition will work on
every operating system. While this means we don't have to include the
above line in the final version of our package definition, I'll put it
in just to be explicit.


@section{Package Inputs}

Now for the interesting stuff. In @|project-name|, a package
definition is a program. An actively running version of that program
is a package. Like any other running program, packages use inputs to
function.


A @deftech{package input} is a deferred request for exact bytes. I'll
define one for now.

@racketmod[#:file "definition.rkt"
xiden

(package "my-first-package")
(provider "sagegerard.com")
(description "Fun playtime in a tutorial")
(tags "fun" "tutorial" "example")
(url "https://sagegerard.com")

(edition "default")
(revision-number 0)
(revision-names "alpha")

(racket-versions ("5.0" "*"))
(os-support unix windows macosx)

(input "default.tgz"
       (sources "https://sagegerard.com/xiden-tutorial/default.tgz"))]

This input defines an archive of source code we'll need to build our
project.  The archive contains a throwaway Racket module and Scribble
document.  @racket{default.tgz} is the name of the file that we use
locally in our build. The @racket[sources] list tells @project-name
where it can find the actual bytes for that file. I'm only using one
source here, but you can add other sources in case one isn't
available.


@subsection{Everything is an Input}

A @tech{package input} can be any file, not just Racket packages or
code.  You can use a Python distribution as an input, or a critical
security patch.  This means that you can use @project-name to build
any software.

While we won't cover it here, another benefit of package inputs is
that you can substitute them. If a build is taking too long because it
compiles a huge project from source, you can adjust the definition to
use pre-built binaries instead.


@subsection{Integrity Information}

We named a file that we want, but how do we know we got the right
file?  For that, we need to declare integrity information with our
input.

@racketblock[
(input "default.tgz"
       (sources "https://sagegerard.com/xiden-tutorial/default.tgz")
       (integrity 'sha384 (hex "299e3eb744725387e0355937727cf6e3c938eda2355cda82d58596fd535188fa624217f52f8c6e7d5ee7cb1d458a7f75")))]

Integrity information tells @project-name if it got the @italic{exact
bytes} the input requires. If it did not, then the build will
fail. This is a good thing because it helps make builds
reproducible. An input might only be available during a build, or may
persist after a build for run-time use. More on that later.

If you are not familiar with integrity checking, just know that there
are functions to take a file and turn it into a fixed-length byte
string called a @italic{digest}.  If two files produce the same
digest, then we can assume the files are the same. That is, unless the
function itself has a @italic{collision}, where two different files
produce the same digest. This is a sign to use a different function!

The function we're using in this case is SHA-384, which we represent
here as @racket[sha384]. Since it's hard to type the exact bytes of a
digest, we can give @project-name the expected digest as a string we
copy and paste from elsewhere.  Here we tell @project-name that the
SHA-384 digest of @racket{default.tgz} comes from a hex string. That
tells @project-name how to translate the digest as a string back to
bytes for comparison.


@subsubsection{Creating an Integrity Expression}

There are a couple of ways you can generate an integrity expression.
Before you try, download the file at the link shown in the integrity
expression in the previous snippet. Our goal is to create the same
expression we saw in the code.

Let's start by making an integrity expression by hand.  If you
followed @secref{setup}, then you should have OpenSSL installed on
your system. You can check the digest for yourself by running this
command:

@verbatim|{
$ openssl dgst -sha384 default.tgz
SHA384(default.tgz)= 299e3eb744725387e...
}|

There's a tricky part here. Yes, the digests match our code, but that
doesn't mean you should paste it right into new definitions. Typically
you want to write a definition using a @italic{trusted copy} of a
file.

We'll assume trust in this file because it is not used to run code on
your system. From here you can write an integrity expression by
hand. Just paste it in this example where you see @racketfont{DIGEST}.

@racketblock[(integrity 'sha384 (hex DIGEST))]

Knowing how to produce your own digest is a valuable skill if you want
to verify data that arrived on your system.  But an external tool
might use a different encoding and algorithm than what @project-name
supports.  If you want the entire integrity expression with options
supported by @|project-name|, then use the @litchar{xiden mkint}
command. This example does the same thing, except you'll get an entire
integrity expression as output.

@verbatim|{
$ xiden mkint sha384 hex default.tgz
(integrity 'sha384 (hex "299e3eb744725387e...
}|

Alternatively, you can tell @project-name to read from standard input.
Just use a dash in place of the file.

@verbatim|{
$ cat default.tgz | xiden mkint sha384 hex -
(integrity 'sha384 (hex "299e3eb744725387e...
}|

Assuming you trust the input, you only need to copy and paste the
expression into your definition. If you want programmatic control over
integrity expressions, then use the @racketmodname[xiden/integrity]
module.


@subsection{Authenticating Inputs}

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
(input "default.tgz"
       (sources "https://sagegerard.com/xiden-tutorial/default.tgz")
       (integrity 'sha384 (hex "299e3eb744725387e0355937727cf6e3c938eda2355cda82d58596fd535188fa624217f52f8c6e7d5ee7cb1d458a7f75"))
       (signature "https://sagegerard.com/xiden-tutorial/public.pem"
                  "https://sagegerard.com/xiden-tutorial/default.tgz.sign"))]

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


@subsection{Abstract and Concrete Package Inputs}

An @deftech{abstract package input} (or just “abstract input”) is a
@tech{package input} with only a name.

@racketblock[(input "server.rkt")]

Such inputs cannot be used to fetch exact bytes, so we won't use them
in our final definition. They are still important to discuss, because
you'll eventually need them.

Abstract inputs are useful as placeholders in package definitions that
require the end user to define inputs. By contrast, we've been writing
@deftech{concrete package inputs}, which are package inputs that
include least one source of bytes. A concrete package input does not
have to include integrity information or a signature.


@section{Package Outputs}

A @deftech{package output} is a named deliverable from the package,
such as documentation, libraries, or tests.

Every package definition should define a default output, because if a
user does not request a particular output from a package, then
@project-name will use the output named @racket{default}.  If you do
not define a default output, then @project-name will tell the user
about the outputs available in the definition.

Recall in the last section that we defined inputs named
@racket{default.tgz}.  We want to fetch and extract that
archive.

@racketblock[
(output "default"
        archive-input <- (input-ref "default.tgz")
        archive-path <- (resolve-input archive-input)
        (extract archive-path)
        (release-input archive-input))
]

When building, @racket[current-directory] is bound to a unique
directory, such that two packages only conflict if evidence shows
those packages will produce identical output.  You can assume the
directory is empty, and yours to populate.

Notice that we manually free the archive using
@racket[release-input]. This is because when you reference an input,
@project-name lazily writes it to disk and issues a symbolic link to
the file with the given name. @project-name cannot predict what inputs
to keep around, so it leaves that to you. We don't need our archive
once the contents are on disk, so we delete the @italic{link} using
@racket[release-input].

We do not delete the actual file because if something goes wrong with
a package, you might not want to fetch every input again.  If there
are no incoming links for a file in @|project-name|, then it is
eligible for garbage collection in a separate process.


@subsection{Monadic Types}

We now have two endpoints to our program, and some processing in
between. So what's the @racket[<-] for in the package output?  If you
are familiar with Haskell and monads, just know that outputs use a
notation similar to @tt{do} and skip this section.

@racket[<-] kind of like @racket[let], but it isn't @italic{exactly}
the same because this abbreviated program does not work.

@racketblock[
(output "default"
        archive-input <- (input-ref "default.tgz")
        (extract (resolve-input archive-input))
        (release-input archive-input))
]

@racket[<-] does bind a value to an identifier, but it also discovers
the value to bind from special context called @deftech{monads}.  There
are many tutorials that explain monads poorly, and this would likely
be one of them. So we'll just focus on an abbreviated introduction
that keeps us moving.

Here are two functions.

@racketblock[
(define (add5 v) (+ v 5))
(define (sub2 v) (- v 2))
]

You can compose them.

@racketblock[
(sub2 (add5 4))
]

Happy days.

One day someone changes the functions so that they return information
to store in a program log.

@racketblock[
(define (add5 v) (values (+ v 5) (format "Adding 5 to ~s" v)))
(define (sub2 v) (values (- v 2) (format "Subtracting 2 from ~s" v)))
]

This is preferable than using @racket[displayln] and the like in
functional programs because this way, calling the function has no
side-effects. Everything returned from each function is expressed
purely in terms of arguments.

Problem is, @racket[(sub2 (add5 4))] no longer works. @tech{Monads}
are just the @italic{stuff} that makes function composition work again
without changing the definition of @racketid[add5] and
@racketid[sub2]. That means something is sitting around that knows
the first value from this kind of function is a number, and the
second value is a string. It knows that composing two such functions
means doing something like this:

@racketblock[
(define (compose-extra f g)
  (define-values (fv fs) (f v))
  (define-values (gv gs) (g fv))
  (values gv (string-append fs "\n" gs)))]

This brings us back to our package output. These instructions work
because @racket[<-] understands how to find the value you want from
all the extra @italic{stuff}.

@racketblock[
archive-path <- (resolve-input "default.tgz")
(extract archive-path)
]

But @racket[(extract (resolve-input archive-input))] doesn't work
because you passed the value you want @italic{plus} the extra
@italic{stuff} to @racket[extract].

In other words, the value returned from @racket[(resolve-input
archive-input)] is @italic{not the same} as the value bound to
@racketid[archive-path] when using @racket[<-].

How you deal with the values depends on the type, which is
normal. You'll pick up on the different types as you go.


@subsection{Adding a Second Output}

Assume @racket{default.tgz} has everything a user would need. Some
users might only want the libraries, not the documentation. Storage is
cheap, but hundreds of dependencies add up.

We can define a new output for our budget-conscious users:

@racketblock[
(output "default"
        archive-path <- (resolve-input "default.tgz")
        (extract archive-path)
        (release-input archive-path))

(output "minimal"
        archive-path <- (resolve-input "minimal.tgz")
        (extract archive-path)
        (release-input archive-path))
]

This version works, but we don't want to repeat ourselves every time
we want a new output. To reduce repetition, we can introduce an
@deftech{action}.  An action is like a procedure, except the body
follows the same notation as outputs.

@racketblock[
(action (unpack name)
  archive-input <- (input-ref name)
  archive-path <- (resolve-input archive-input)
  (unpack archive-path)
  (release-input archive-input))

(output "default" (unpack "default.tgz"))
(output "minimal" (unpack "minimal.tgz"))
]


By adding an output, we changed what the user can request from a
package.  Since the build extracts an archive with the same name as
the output, we'll need a new @tech{package input} to go with this
output.

@racketblock[
(input "default.tgz"
       (sources "https://sagegerard.com/xiden-tutorial/default.tgz")
       (integrity 'sha384 (hex "299e3eb744725387e0355937727cf6e3c938eda2355cda82d58596fd535188fa624217f52f8c6e7d5ee7cb1d458a7f75"))
       (signature "https://sagegerard.com/xiden-tutorial/public.pem"
                  "https://sagegerard.com/xiden-tutorial/default.tgz.sign"))

(input "minimal.tgz"
       (sources "https://sagegerard.com/xiden-tutorial/minimal.tgz")
       (integrity 'sha384 (hex "6cc38a7e2513fa9abd2ac079e9c8efbab9385458275c927e77527a189ed9ac393d734a4cf306787425bf722a5ac025c6"))
       (signature "https://sagegerard.com/xiden-tutorial/public.pem"
                  "https://sagegerard.com/xiden-tutorial/minimal.tgz.sign"))]

Now when our users choose to build @racket{minimal} output, they will
only ever download and extract the @racket{minimal.tgz} archive.


@subsection{Outputs Can Create Duplicate Data}

@project-name assumes that different outputs produce different
content, which makes one kind of human error possible.

Assume we add an output to act as an alias of another.

@racketblock[
(output "default" (unpack "default.tgz"))
(output "full" (unpack "default.tgz"))
(output "minimal" (unpack "minimal.tgz"))]

This works, but if a user requests the @racket{full} output and then the
@racket{default} output, then the same archive would be extracted twice into
different directories.  This pollutes the disk with redundant data, which is
probably not what you want.

Different @tech{package outputs} are expected to produce different
things. If you believe that two outputs are equivalent, then combine
them into one output. If multiple outputs end up creating too much
duplicate data, then you might want to consider defining the common
data in another package definition.


@section{User-defined Metadata}

A lot of what we've added to our code counts as metadata, such as
@tt{url}, @tt{tags}, and @tt{description}. All of the entries
we've defined so far are metadata that @project-name readily
recognizes due to their widespread use.

If you want to store other information in your definition,
then you can use the @tt{metadatum} form.

@racketblock[
(metadatum support-email "support@example.com")
]

A metadatum works like @tt{define}, in that you can bind one
identifier to a value. When someone uses @racket[require] or one of
its variants on a package definition, they can inspect an expanded
@tt{metadata} binding to see all user-defined metadata.

@racketinput[(module anon xiden/pkgdef2 (metadatum support-email "support@example.com"))]
@racketinput[(require 'anon)]
@racketinput[metadata]
@racketresult['#hasheq((support-email . "support@example.com"))]

Metadata can only be literal strings, and are not meant for use in
program logic.


@section[#:tag "finished-definition"]{The Finished Definition}

Here is the file we've authored. To recap, it defines a build that simply
extracts an archive depending on the requested output. We'll discuss this
definition further in @secref{cli}.


@racketmod[#:file "definition.rkt"
xiden

(package "my-first-package")
(provider "example.com")
(description "Fun playtime in a tutorial")
(tags "fun" "tutorial" "example")
(url "https://sagegerard.com")

(edition "default")
(revision-number 0)
(revision-names "alpha")

(code:comment "-----------------------------------------------")
(code:comment "Platform support")
(racket-versions ("5.0" "*"))
(os-support unix windows macosx)

(code:comment "-----------------------------------------------")
(code:comment "User Metadata")
(metadatum support-email "support@example.com")


(code:comment "-----------------------------------------------")
(code:comment "Inputs")
(input "default.tgz"
       (sources "https://sagegerard.com/xiden-tutorial/default.tgz")
       (integrity 'sha384 (hex "299e3eb744725387e0355937727cf6e3c938eda2355cda82d58596fd535188fa624217f52f8c6e7d5ee7cb1d458a7f75"))
       (signature "https://sagegerard.com/xiden-tutorial/public.pem"
                  "https://sagegerard.com/xiden-tutorial/default.tgz.sign"))

(input "minimal.tgz"
       (sources "https://sagegerard.com/xiden-tutorial/minimal.tgz")
       (integrity 'sha384 (hex "6cc38a7e2513fa9abd2ac079e9c8efbab9385458275c927e77527a189ed9ac393d734a4cf306787425bf722a5ac025c6"))
       (signature "https://sagegerard.com/xiden-tutorial/public.pem"
       		  "https://sagegerard.com/xiden-tutorial/minimal.tgz.sign"))

(code:comment "-----------------------------------------------")
(code:comment "Outputs")

(action (unpack name)
  archive-input <- (input-ref name)
  archive-path  <- (resolve-input archive-input)
  (unpack archive-path)
  (release-input archive-input))

(output "default" (unpack "default.tgz"))
(output "minimal" (unpack "minimal.tgz"))
]
