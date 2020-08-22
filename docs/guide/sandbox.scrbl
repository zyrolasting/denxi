#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Sandboxing Packages}

If you are suspicious of an installed package, you can start a
sandboxed REPL in the context of a package using the @tt{sandbox}
command.

@verbatim|{
$ xiden sandbox mavrick:pyracket:totally-works
>
}|

By default, the sandbox prohibits all file and network I/O, and limits
both the time and memory the package module can consume. Since the
sandbox uses your kernel, it is not a substitute for OS-level
security.  See @racketmodname[racket/sandbox] for more information.
