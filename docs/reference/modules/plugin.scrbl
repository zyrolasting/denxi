#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/function
                    xiden/catalog
                    xiden/rc
                    xiden/source
                    xiden/package
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
Evaluates as @racket[(dynamic-require P key fail-thunk)], where
@racketid[P] is the path to the @tech{plugin}.

If the plugin fails to load, @racket[load-from-plugin] returns the
result of @racket[on-load-failure] applied to the relevant exception.
}

@defproc[(plugin-ref [key symbol?] [default-value any/c]) any/c]{
Equivalent to @racket[(load-from-plugin key (const default-value) (const default-value))].
}

@section{Supported Bindings}

Xiden applies @racket[load-from-plugin] internally to get
references to user-defined extensions. The bindings defined below are
not provided by Xiden, but are instead provided by a
@tech{plugin} to support the written behavior.

@defthing[canonical-catalog catalog?]{
A @tech{catalog} used to resolve package queries.

The catalog instance is considered authoratative.  You may incorporate
many catalogs into that one instance.

Defaults to @racket[(get-default-catalog)].
}

@defproc[(string->source [user-string string?]) source?]{
Converts a string to a @tech{source}.

When a user uses a plain string where a source is expected, then Xiden
uses @racket[coerce-source] to convert it to a source. By defining
this, you control how strings map to byte sources.

Defaults to

@racketblock[
(define (string->source s)
  (first-available-source
   (list (file-source s) (http-source s))
   null))
]
}

@defproc[(verify-signature [intinfo integrity-info?]
                           [public-key-path path-string?]
                           [signature-path path-string?]) boolean?]{
Returns @racket[#t] if Xiden may assume that the signature was signed
by a private key corresponding to the public key. Making this
procedure always return @racket[#t] is equivalent to setting
@racket[XIDEN_TRUST_UNSIGNED] to @racket[#t].

The default implementation is a procedure that uses the host's OpenSSL
installation to verify the signature.

@racket[intinfo] is set to the integrity information for the signed
data.

You may assume that the public key is trusted if control reaches this
procedure in the context of a command line invocation.
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


@defproc[(override-package [p package?] [arg any/c] ...) package?]{
Replaces one package with another. Xiden applies
@racket[override-package] when evaluating @racket[comission] terms in
a package definition. The first argument is always a package with a
state built by preceding terms. If you do not wish to impact the
package at all, just return @racket[p].

The remaining arguments (be they keyword arguments or formal
arguments) depend on the implementation of @racket[override-package],
since @racket[comission] simply forwards the user's arguments to it.

@racket[override-package] is useful for computing a complete package
definition in terms of limited data and prescribed conventions. It may
be easier to define @racket[override-package] than to write a new
module language or reader extension.
}
