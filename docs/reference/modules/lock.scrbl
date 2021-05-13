#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/string
                    xiden/dig
                    xiden/input
                    xiden/message
                    xiden/notary
                    xiden/openssl
                    xiden/package
                    xiden/pkgdef/static
                    xiden/racket-module
                    xiden/source
                    xiden/string
                    xiden/subprogram
                    xiden/url
                    xiden/version]
         "../../shared.rkt"]

@title{Lockfiles}

@defmodule[xiden/lock]

A @deftech{locked package definition}—or simply “@deftech{lockfile}”
in the context of this section—is a @tech{package definition} with raw
bytes expressed wherever a @tech{source} may be found.  Lockfiles
guarentee reproducible transactions against a @tech{state} so long as
the lockfile can fit in memory.  When lockfiles differ from an
original package definition, they require new integrity and/or
signature information to work with a zero-trust configuration. To do
this, a @tech{notary} creates digests and signatures.

The locking process is recursive, such that if a package definition
uses another package definition as an input, then @italic{that}
package definition is locked and embedded in the containing package
definition. The result is a self-contained software distribution
program.

Lockfiles are loaded entirely into memory when built or used, so they
should be kept reasonably small in practice. Unfortunately, the first
implementation of this module allows duplicate data in lockfiles
(e.g. the same public key will appear multiple-times).  Lockfile size
may increase faster than you'd expect until this improves.


@defproc[(lock-package-definition [package-definition-source source?]
                                  [notary notary? lazy-notary])
                                  (subprogram/c bare-pkgdef?)]{
Returns a @tech{subprogram} that converts a @tech{package definition}
from @racket[package-definition-source] into a @tech{lockfile}. The
subprogram finishes with a @tech{bare} variant of the @tech{lockfile}.

This process will recurse to other package definitions named in the
inputs.  A cycle detection mechanism will halt the subprogram with
@racket[$cycle] if the content for any two package definitions produce
the same digest using @racket[DEFAULT_CHF], @italic{not} @racket[chf].

Integrity information and signature information are regenerated for
all artifacts using @racket[notary].

The subprogram can be influenced by @racket[current-package-editor],
@racket[current-package-definition-editor], @racket[current-shovel],
and safety limits under the @tech{runtime configuration}.
}
