#lang scribble/manual

@require["../../shared.rkt"
         xiden/workspace
         @for-label[racket/base
                    racket/contract
                    racket/path
                    xiden/workspace]]

@title{Workspaces}

@defmodule[xiden/workspace]

A @deftech{workspace} is a directory where Xiden performs all file
I/O. That directory's name is either @wsdir, or @racket[(file-name-from-path
(getenv "XIDEN_WORKSPACE"))] (See @secref{workspace-selection}). The path to a
workspace is defined in @racket[workspace-directory], and must comply with
@racket[workspace-directory/c].

A @deftech{target workspace} is the directory referenced by the value
of @racket[(workspace-directory)].

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


@defthing[workspace-directory (parameter/c workspace-directory/c)]{
The directory in which Xiden reads and writes files.

If the directory does not exist, then it will be created when Xiden writes a file.
}

@defthing[CONVENTIONAL_WORKSPACE_NAME string?]{
Bound to @racketfont{"@CONVENTIONAL_WORKSPACE_NAME"}. Use the identifier instead of a
hard-coded string to track changes in conventions.
}

@defstruct*[($invalid-workspace-envvar $message) () #:prefab]{
A @tech{message} indicating that @racket[(getenv "XIDEN_WORKSPACE")]
is not useable as a @tech{workspace} path.
}

@defproc[(build-workspace-path [path-element (and/c path-string? (not/c complete-path?))]) complete-path?]{
Like @racket[build-path], but the base of the returned path is
@racket[(workspace-directory)].
}

@defproc[(call-with-ephemeral-workspace [proc (-> path? any)]) any]{
Calls @racket[proc] in a @tech/reference{parameterization} where
@racket[(current-workspace)] is a temporary directory. The same path
is passed as the first argument to @racket[proc]. That directory and
its contents will be deleted when control leaves @racket[proc], if it
still exists.
}

@section[#:tag "workspace-selection"]{Selecting a Workspace}

When Xiden starts, it selects a value to install in
@racket[workspace-directory] using the following rules.

@margin-note{The @tt{XIDEN_WORKSPACE} environment variable is the only way to
define a workspace with a non-conventional name.}
Let @racket[W] be @racket[(getenv "XIDEN_WORKSPACE")].  If
@racket[(workspace-directory/c W)] is @racket[#t], then
@racket[workspace-directory] is set to @racket[W].

If @racket[(workspace-directory/c W)] is @racket[#f], then Xiden will
print an error (but not halt) if @racket[W] is set to a non-empty string.

If @racket[W] was not used, then Xiden will walk towards a root
directory starting from @racket[current-directory] in search of an existing
@|wsdir|. If one is found, then @racket[workspace-directory] is set to its
path.

If no directories were found in the walk towards a root directory, then
@racket[workspace-directory] is set to @racket[(build-path (current-directory)
CONVENTIONAL_WORKSPACE_NAME)].

@section{State Management}

@defmodule[xiden/localstate]

@subsection{Garbage Collection}

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
