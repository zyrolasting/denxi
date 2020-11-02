#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base xiden/rc]]

@title[#:tag "security"]{Security}

A @project-name process operates under the permissions granted to it
by the operating system. @project-name offers no extensions or
modifications to the security model of your operating system. Run
packages at your own risk.

Assuming reasonably safe OS permissions, a @|project-name| process
uses a @tech/xiden-reference{runtime configuration} to store the
user's detailed, explicit, and affirmative consent to specific
operations. If the runtime configuration is compromised, then a
@project-name process is restricted @italic{only} by its OS-level
permissions.

The first @project-name instance you install implicitly trusts its own
dependencies. It is your responsibility to use @project-name with
trusted copies of the dependencies listed in
@secref{setup}. Self-hosted installations may verify dependencies in
terms of the @tech/xiden-reference{runtime configuration}. Assuming
the dependencies can be trusted, builds are run in the context of
@racketmodname[racket/sandbox].

The attack surface boils down to:

@itemlist[
@item{The permissions set on any Racket process that can use @|project-name|'s bindings.}
@item{The @tech/xiden-reference{runtime configuration} used by @project-name in such a process.}
]
