#lang scribble/manual

@require["../shared.rkt" @for-label[@except-in[xiden/pkgdef #%module-begin] racket/base]]

@title{Abstract Package I/O}

An @deftech{abstract package input} (or just “abstract input”) is a
package input with only a name.

@racketblock[(input "server.rkt")]

Abstract inputs are at the mercy of a @tech/xiden-guide{launcher}
or an input override to provide meaning. This is useful when you
require an end user to define their own implementation for
well-defined interfaces, or to provide custom input for builds.

There are also @deftech{abstract package outputs}, which define no
steps to produce files.

@racketblock[(output "default")]

Abstract outputs are obviously useless for file creation. However,
abstract outputs still have a name, and output names are the last
piece of information Xiden needs to check if something is already
installed. This way if you have an existing
@tech/xiden-reference{workspace} with some pre-installed packages, use
an abstract output to induce a package conflict and quickly issue new
links to the dependency on your system.

This enables certain workflows where you write a package definition to
install some software, check the workspace into version control, and
then push up the workspace. Even if you forgot to check the package
definition into source control, someone only needs to know the output
name to access the files.
