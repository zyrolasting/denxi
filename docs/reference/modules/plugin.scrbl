#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    xiden/rc
                    xiden/source
                    xiden/plugin]
                    "../../shared.rkt"]

@title{Plugins}

@defmodule[xiden/plugin]

A @deftech{plugin} is a Racket module defined by a user.  Plugins
extend @project-name where it would otherwise have limited behavior.
A @tech{plugin} runs directly in @|project-name|'s runtime, with the
same level of privilege as the OS-level user. This means that a plugin
may freely reconfigure @|project-name|, if not outright harm a system.

@defproc[(load-from-plugin [key symbol?] [fail-thunk (-> any)] [on-load-failure (-> exn:fail? any)]) any]{
Evaluates as @racket[(dynamic-require (XIDEN_PLUGIN_MODULE) key fail-thunk)].

If @racket[(XIDEN_PLUGIN_MODULE)] fails to load,
@racket[load-from-plugin] returns the result of
@racket[on-load-failure] applied to the relevant exception.
}

@section{Supported Bindings}

@project-name applies @racket[load-from-plugin] internally to get
references to user-defined extensions. The bindings defined below are
not provided by @project-name, but are instead provided by a
@tech{plugin} to support the written behavior.

@defproc[(fetch-source [source string?] [request-transfer request-transfer/c]) any/c]{
This procedure must apply @racket[request-transfer] (See
@racket[request-transfer/c]) in tail position to copy bytes in terms
of a @tech{source} that @project-name does not understand.

@racket[fetch-source] may return @racket[#f] or raise an exception
if it cannot function as expected.
}
