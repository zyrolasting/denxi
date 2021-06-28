#lang scribble/manual

@require["../shared.rkt"]

@title[#:tag "launcher-security"]{Launcher Security}

Xiden's default configuration can be thought of as “Deny All”, but a
custom launcher is any Racket module that starts with all privileges
granted by the operating system. Any restrictions Xiden sets in terms
of the configuration will only apply when the launcher actually gives
control to Xiden. So, if your custom launcher is compromised, then
Xiden is compromised.

You can use the built-in @litchar{xiden} launcher to mitigate some
risks. As stated, it has the same interface as other launchers.

@verbatim|{
$ xiden do +d vendor definition.rkt
}|

The key difference is that the @litchar{xiden} launcher does not allow
access to some @tech/reference{parameters}, and is only configurable
by command-line flags and environment variables. This reduces the size
of the attack surface, at the cost of flexibility.

Custom launchers can do things that the default launcher cannot, such
as intelligently respond to package conflicts, extend the CLI, or
resolve @tech{package queries} in specific ways. The good news is that
launchers can themselves be signed and distributed using Xiden, to
protect trust in how Xiden launches despite the customizations.

For more information, see @topic{securing-launch}.
