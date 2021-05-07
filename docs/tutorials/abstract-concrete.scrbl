#lang scribble/manual

@require["../shared.rkt" @for-label[@except-in[xiden/pkgdef #%module-begin] racket/base]]

@title{Abstract and Concrete Package Inputs}

An @deftech{abstract package input} (or just “abstract input”) is a
@tech/xiden-guide{package input} with only a name.

@racketblock[(input "server.rkt")]

Abstract inputs are at the mercy of a @tech/xiden-guide{launcher}
or an input override to provide meaning. This is useful when you
require an end user to define their own implementation for
well-defined interfaces, or to provide custom input for builds.

Xiden prohibits overriding abstract inputs with other abstract inputs,
and any override is subject to the same runtime restrictions.
