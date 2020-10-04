#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base xiden/rc]]

@title[#:tag "security"]{Security}

This section covers how @project-name impacts the security of your system.

First understand that @bold{@project-name does not guarentee security}.  Since
it can run programs from the Internet, you need to manage the risks.


@section{Caution is Configurable}

Certain settings like @racket[XIDEN_TRUST_ANY_PUBLIC_KEY] or
@racket[XIDEN_TRUST_BAD_DIGEST] can be changed to make @|project-name| more or
less vigilant. One might weaken @|project-name|'s checks to test package
distribution, or to reduce overhead when sharing data in a trusted network.

It goes without saying that these settings should not be changed
lightly. @racket[XIDEN_TRUST_BAD_DIGEST] is actually the most dangerous setting
because trusting malformed input means trusting any code or data for a
build. This makes most other checks pointless.  Since runtime configuration
determines the security of a transaction, special care must be taken to protect
the sources of runtime configuration. @|project-name|'s default configuration
offers no trust, meaning that any issue with an input's integrity, signature,
associated public key/server certificate will cause @project-name to abort and
roll back the entire transaction. This is done only to put the onus on the user
to declare their level of trust for transactions.


@section{Use Operating System Permissions}

@project-name does not control operating system security.  It uses
@racketmodname[racket/sandbox] to restrict the behavior of builds in the Racket
runtimeq, but this does not offer any actual overall protection.  Non-Racket
programs installed or executed by @project-name also do not have to obey the
restrictions set by a Racket runtime, which grants a natural privilege
escalation to whatever permissions are set on the user account. This means that
a @tech{package definition} can, for example, run a subprocess that tries to
change @|project-name|'s runtime configuration.

The confidence you place in the behavior of a @project-name process should come
from its permissions on the operating system. Do not run @project-name as a
highly-privileged user unless you understand and accept the consequences.


@section{Consent Applies to Transactions}

@project-name offers integrity and signature checking to help you declare trust
in third-party data. Put another way, @project-name will only process
transactions with your affirmative consent. This means that your decision to
declare unsigned inputs (for example) applies to @italic{every} unsigned input
encountered in the transaction. This makes high-trust configurations especially
dangerous for @tech{package definitions} with many transient dependencies.

Build graphs can be so complicated that no human can vet every dependency
without mistakes.  If you wish to apply consent to each input, use an auditing
process aided by a low-trust configuration: Run a transaction with minimal
trust, and then respond to any particular input flagged as suspicious. This
helps a user interactively navigate and manage the build graph.

The use of integrity information and signatures also allows a user to
@italic{take responsibility} for inputs that initially lacked a signature.
This means that if an otherwise trusted package uses an unsigned input,
a user might put their own signature on a modified definition to show
that they've audited the input and take responsibility for its behavior
in the context of a project.
