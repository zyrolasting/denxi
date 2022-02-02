#lang scribble/manual

@require[@for-label[racket/base]
         "../shared.rkt"]

@title[#:style '(toc)]{Denxi Guide}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

Here you learn what Denxi is, what it can do for you, and how to
practice using it.

For all documentation, see @other-doc[denxi-index].

@section{Introduction}

Denxi is a programming model for shipping and receiving untrusted data
according to the receiving party's rules. Denxi reduces the cost of
producing package managers, storefronts, operating systems, and CI/CD
systems by solving the same distribution problems with a detailed
configuration model. For more, see @other-doc[denxi-white-paper].

Why is this useful? When someone ships a package to your house, they
don't stick around to unpack the box, buy other things, and mess with
your house. Not without asking you first, I'd hope. Denxi is more like
a respectful delivery driver: It will ship data to you according to
your rules, and then leave.  You have the tools to handle the
receiving side, including ways to easily handle dependencies when you
realize batteries weren't included with your new toys.

Denxi is hard to summarize due to its broad applicability, so this
guide is for developers. @other-doc[denxi-reference] covers hundreds
of bindings for many scenarios that come up when a user and a provider
try to cut a deal. This guide briefly introduces core concepts, then
directs you to concrete examples for practice. Experts who make it
through all published materials are in a position to set up their own
communities or clientele using custom launchers and catalogs.


@section{Setup}

@hyperlink["https://download.racket-lang.org/"]{Download} and install
Racket, then run @litchar|{raco pkg install --force denxi}|.


@section{Catalog}

A @deftech{catalog} is a program written from the perspective of
someone who wants to distribute data.  Catalogs offer
@tech/denxi-reference{artifacts} as verifiable bytes.  A catalog is
subject to user-defined safety limits (such as a maximum size in bytes
for the catalog itself), and may have no side-effects.

@racketmod[#:file "definition.rkt"
denxi/catalog

(metadatum support-email "support@example.com")

(offer "hello.txt" (artifact (text-source "Hello!")))
(offer "question.txt" (artifact (text-source "How are you?")))
(offer "goodbye.txt" (artifact (text-source "Goodbye!")))
]

In this sense, a catalog defines both the shipper's inventory and
the set of possible packages that Denxi can build using the artifacts.
Catalogs create dependency relationships by expecting the user to have
compatible items in their own inventory.

Here, “inventory” refers to a single namespace shared by artifacts on
the user's system, regardless of storage medium. Each user acts as
their own naming authority, so the catalog does not have to worry
about name conflicts on the user's system. Denxi tracks the
relationship between catalog-defined names and user-defined names to
disambiguate what a given name references.

@racketmod[#:file "definition.rkt"
denxi/catalog

(metadatum support-email "support@example.com")

(offer "hello.txt" (artifact (text-source "Hello!")))
(offer "question.txt" (artifact (text-source "How are you?")))
(offer "goodbye.txt" (artifact (text-source "Goodbye!")))

(offer "hello+goodbye" (bundle "hello.txt" "goodbye.txt"))
]


@section{Launchers}

A @deftech{launcher} is a trusted program used as an entry point for
Denxi. The @racketmodname[denxi/launcher] language helps developers
write launchers. Launchers prepare transactions with catalogs by
naming the artifacts they wish to accept, and mapping catalog-defined
names to user-defined names. Denxi accepts orders on behalf of the
receiver, fetches the artifacts while wearing white gloves, and ships
the artifacts to the receiver as a dynamically-generated package.

Launchers are valuable because they impose a receivers' semantics on
shippers, such that all challenges related to software deployment are
outside of the shipper's control. All a shipper needs to do is publish
their own data, leave some notes, and sit back. Shippers may even
share their own launchers to help users access content without added
work, but users retain the ability to ignore or audit those launchers.

@racketmod[#:file "my-denxi.rkt"
denxi/launcher

(module+ main
  (denxi ...
         (order (artifact ...)
	        "hello.txt"
		"greeting")))
]

The @racket[denxi] procedure performs a transaction on state with a
configuration. The configuration defaults to a position of
@italic{zero trust}. For Denxi to function, you must explicitly define
@italic{all settings} the transaction requires to function. This may
make the entry point large and complex, but you can always divide
sections of a launcher into a catalog for distribution. Maximally
insecure transactions imply passing an ill-advised value for every
argument, that a neutral programmer would recognize as such.

The default value of any setting is biased toward stopping operation
because the act of using a default implies a lack of user awareness,
and possibly consent. The zero-trust defaults are hard-coded in Denxi,
and all settings only ever used as arguments to pure functions
internally.


@section{Practice}

@(define ex0 "https://github.com/zyrolasting/denxi/tree/master/examples/generated-racket-bindings")
@(define ex1 "https://github.com/zyrolasting/denxi/tree/master/examples/cryptography-backends")
@(define ex2 "https://github.com/zyrolasting/denxi/tree/master/examples/output-conflicts")
@(define ex3 "https://github.com/zyrolasting/denxi/tree/master/examples/racket-installation-manager")

This guide briefly showed you toy examples of the two primary
abstractions used in Denxi. Toy examples are never enough, but it
helps to know that @italic{everything boils down to launchers and
catalogs}.

From here, you have a choice. You can read
@other-doc[denxi-white-paper] to understand why Denxi is worth using,
or study concrete examples. Examples include @hyperlink[ex1]{defining
a host's OpenSSL instance as a cryptographic backend},
@hyperlink[ex2]{resolving package conflicts locally},
@hyperlink[ex3]{managing multiple Racket versions}, and
@hyperlink[ex0]{forcing one set of generated bindings}.

You may wish to skip these examples and start reading
@other-doc[denxi-reference], but you'll struggle to navigate the
reference without the examples or this guide to set context.

You may review the examples through the links above, but all of the
examples have been shipped to you when you installed Denxi. Run this
command to print the examples directory on your disk. Go to that
directory and look for a README. The README picks up immediately after
this guide, and shows you a suggested reading order.

@verbatim[#:indent 2]|{
racket -e '(~a (collection-file-path "examples" "denxi"))'
}|

With enough practice you will write your own catalogs and launchers to
distribute. If you manage to do so, then thank you for using Denxi!
