#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base xiden/rc]]

@title[#:tag "security"]{Security}

A Xiden process operates under the permissions granted to it by the
operating system. @bold{Xiden offers no extensions or modifications to
the security model of your operating system. Run packages at your own
risk.}

Assuming reasonable OS permissions, Xiden's attack surface consists of
the implicit trust placed in dependencies used during @secref{setup},
and the @tech/xiden-reference{plugin} for any process using Xiden.
