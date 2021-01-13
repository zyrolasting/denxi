#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    xiden/rc
                    xiden/source
                    xiden/plugin
                    xiden/racket-module]
                    "../../shared.rkt"]

@title{Plugins}

@defmodule[xiden/plugin]

A @deftech{plugin} is a Racket module defined by a user.  Plugins
extend Xiden where it would otherwise have limited behavior.
A @tech{plugin} runs directly in Xiden's runtime, with the
same level of privilege as the OS-level user. This means that a plugin
may freely reconfigure Xiden, if not outright harm a system.

@defproc[(load-from-plugin [key symbol?] [fail-thunk (-> any)] [on-load-failure (-> exn:fail? any)]) any]{
Evaluates as @racket[(dynamic-require (XIDEN_PLUGIN_MODULE) key fail-thunk)].

If @racket[(XIDEN_PLUGIN_MODULE)] fails to load,
@racket[load-from-plugin] returns the result of
@racket[on-load-failure] applied to the relevant exception.
}

@section{Supported Bindings}

Xiden applies @racket[load-from-plugin] internally to get
references to user-defined extensions. The bindings defined below are
not provided by Xiden, but are instead provided by a
@tech{plugin} to support the written behavior.


@defproc[(bind-custom-fetch [hint any/c] [request-transfer request-transfer/c] [fail (-> (or/c exn? $message?) any)]) procedure?]{
@racket[bind-custom-fetch] returns a procedure (@racket[custom-fetch])
that must accept the keyword arguments and formal arguments from a
@racket[plugin-source]. @racket[hint] can be used to select a specific
procedure to return.

@racket[custom-fetch] must apply either @racket[request-transfer] or
@racket[fail] in tail position depending on whether bytes can be
produced in terms of the arguments. Any value @racket[raise]d by
either @racket[bind-custom-fetch] or @racket[custom-fetch] will be
caught and passed to @racket[fail].
}


@defproc[(get-extract-procedure [path path-string?]) (-> input-port? void?)]{
Return a procedure used to extract files from the archive located at
@racket[path], or @racket[#f] if no such procedure is available.

Used by @racketmodname[xiden/archive] when it cannot extract files
from @racket[path] on its own.
}


@defproc[(before-new-package [original bare-racket-module?]) bare-racket-module?]{
A hook for returning a @tech{bare} @tech{package definition} to use
for creating a @tech{package}. @racket[original] represents a
@tech{package definition} fetched from a @tech{source}.

This procedure defaults to the identity function,
which means no code is replaced. Otherwise, you may return an
alternative definition to override @racket[original].

This procedure is useful for standardizing definitions, or for
analyzing builds in a @tech{workspace}. @bold{Define with care.} This
procedure can override every package definition, which can render a
Xiden process inoperable or unsafe.

Take for example a function that returns the same static package
definition, which has one dependency.

@racketblock[
(define (before-new-package original)
  (struct-copy bare-racket-module original
               [code
                 '((input "pkgdef" (sources "https://example.com/other.rkt"))
                   (output "default"
                           pkgdef-input := (input-ref "pkgdef")
                           pkgdef-path := (resolve-input "pkgdef")
                           (install #f #f pkgdef-path)))]))
]

This creates builds that will not terminate. Even if Xiden downloads a
new package definition from @racket{https://example.com/other.rkt}, it
will only be replaced by another instance of the same data returned
from @racket[before-new-package].
}
