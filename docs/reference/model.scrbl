#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Model}

@project-name builds dependencies atomically and deterministically
using @tech{package definitions}.  A package definition contains
discovery information and a declaration of program structure. The
program structure consists of @tech{package inputs}, package outputs,
and a package build procedure for processing. In this sense, a package
is a branded program that reliably reproduces exact files.

All of @|project-name|'s files are stored in a @tech{workspace}
directory. Barring direct user modifications, the content of a workspace
directory is always internally consistent due to transactional file I/O.

@project-name issues symbolic links as references to built files and
directories. This is how a dependent gains access to a dependency.
@|project-name| implements a garbage collector that deletes any file or
directory with no known symbolic links.
