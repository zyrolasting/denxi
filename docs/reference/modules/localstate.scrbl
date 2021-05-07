#lang scribble/manual

@require["../../shared.rkt"
         @for-label[racket/base
                    racket/contract
                    racket/path
                    xiden/localstate]
        xiden/localstate
        @for-syntax[xiden/localstate]]

@title{State}

@defmodule[xiden/localstate]

Xiden performs all file I/O in a single deftech{workspace} directory
that matches the @racket[workspace-directory/c] contract. A
@deftech{target workspace} is the directory referenced by the value of
@racket[(XIDEN_WORKSPACE)].

@defthing[workspace-directory/c contract?
          #:value (and/c complete-path?
                         (or/c directory-exists?
                               (and/c (not/c file-exists?)
                                      (not/c directory-exists?)
                                      (not/c link-exists?))))]{
A contract for a valid workspace directory path.

That is, a complete path that either refers to an existing
directory, or a location on the filesystem where nothing exists.
}



@defsetting*[XIDEN_WORKSPACE]{
The directory in which Xiden reads and writes files.

If the directory does not exist, then it will be created when Xiden writes a file.
}

@defproc[(build-workspace-path [path-element (and/c path-string? (not/c complete-path?))]) complete-path?]{
Like @racket[build-path], but the base of the returned path is
@racket[(XIDEN_WORKSPACE)].
}

@defproc[(call-with-ephemeral-workspace [proc (-> path? any)]) any]{
Calls @racket[proc] in a @tech/reference{parameterization} where
@racket[(XIDEN_WORKSPACE)] is a temporary directory. The same path
is passed as the first argument to @racket[proc]. That directory and
its contents will be deleted when control leaves @racket[proc], if it
still exists.
}

@section{Garbage Collection}

@defstruct*[($finished-collecting-garbage $message) ([bytes-recovered exact-nonnegative-integer?]) #:prefab]{
A message for reporting the number of bytes freed from disk using @racket[xiden-collect-garbage].
}

@defproc[(xiden-collect-garbage) exact-nonnegative-integer?]{
Deletes all records of links that do not actually exist on disk, and
deleltes any installed files or directories in the @tech{target
workspace} with no referencing links.

Returns the estimated number of bytes recovered from disk. This number
is not fully accurate, because of inaccuracies from
@racket[file-size], and the fact that empty directories and links are
treated as negligibly small.
}
