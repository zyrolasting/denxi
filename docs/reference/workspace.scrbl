#lang scribble/manual

@require["../shared.rkt"
         xiden/workspace
         @for-label[racket/base
                    racket/contract
                    racket/path
                    xiden/workspace]]

@title{Workspace}

@defmodule[xiden/workspace]

A @deftech{workspace} is a directory where @project-name performs all file
I/O. That directory's name is either @wsdir, or @racket[(file-name-from-path
(getenv "XIDEN_WORKSPACE"))] (See @secref{workspace-selection}). The path to a
workspace is defined in @racket[workspace-directory], and must comply with
@racket[workspace-directory/c].

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
The directory in which @project-name reads and writes files.

If the directory does not exist, then it will be created when @project-name writes a file.
}

@defthing[CONVENTIONAL_WORKSPACE_NAME string?]{
Bound to @racketfont{"@CONVENTIONAL_WORKSPACE_NAME"}. Use the identifier instead of a
hard-coded string to track changes in conventions.
}

@section[#:tag "workspace-selection"]{Selecting a Workspace}

When @project-name starts, it selects a value to install in
@racket[workspace-directory] using the following rules.

@margin-note{The @tt{XIDEN_WORKSPACE} environment variable is the only way to
define a workspace with a non-conventional name.}
Let @racket[W] be @racket[(getenv "XIDEN_WORKSPACE")].  If
@racket[(workspace-directory/c W)] is @racket[#t], then
@racket[workspace-directory] is set to @racket[W].

If @racket[(workspace-directory/c W)] is @racket[#f], then @project-name will
print an error (but not halt) if @racket[W] is set to a non-empty string.

If @racket[W] was not used, then @project-name will walk towards a root
directory starting from @racket[current-directory] in search of an existing
@|wsdir|. If one is found, then @racket[workspace-directory] is set to its
path.

If no directories were found in the walk towards a root directory, then
@racket[workspace-directory] is set to @racket[(build-path (current-directory)
CONVENTIONAL_WORKSPACE_NAME)].