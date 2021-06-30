#lang scribble/manual

@require["../../shared.rkt"
         @for-label[racket/base
                    racket/contract
                    racket/path
                    racket/sequence
                    xiden/codec
                    xiden/message
                    xiden/query
                    xiden/state
                    xiden/string
                    xiden/port
                    xiden/version]
        xiden/state
        @for-syntax[xiden/state]]

@title{State}

@defmodule[xiden/state]

Xiden's @deftech{state} depends on the contents of a filesystem
directory, and a @tech{database} in that
directory. @racketmodname[xiden/state] encapsulates state I/O.

Xiden implicitly trusts a state, so @bold{directly tampering with
state is a bad idea.} Only use @racketmodname[xiden/state] to interact
with a @tech{state}.


@section{Workspaces}

A @deftech{workspace} is a directory on the filesystem that holds a
@tech{database} and all installed software. A workspace's path must
match the @racket[workspace-directory/c] contract. A @deftech{target
workspace} is the directory referenced by the value of
@racket[(XIDEN_WORKSPACE)]. Target workspaces are affected by all
filesystem writes in a Xiden process.

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


@defthing[#:kind "setting" XIDEN_WORKSPACE workspace-directory/c]{
CLI Flags: @litchar{--w/--workspace}

The directory in which Xiden reads and writes files. If the directory
does not exist, then it will be created when Xiden writes a file.

Defaults to @racket[(build-path (find-system-path 'home-dir) ".xiden")].
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


@defproc[(path-in-workspace? [path path-string?]) boolean?]{
Returns @racket[#t] if @racket[path], when simplified, has
@racket[(XIDEN_WORKSPACE)] as a prefix.
}


@defproc[(make-addressable-file [transfer-name non-empty-string?]
                                [in input-port?]
                                [est-size (or/c +inf.0 exact-positive-integer?)]
                                [#:on-status on-status (-> $message? any)]
                                [#:max-size max-size (or/c +inf.0 exact-positive-integer?)]
                                [#:buffer-size buffer-size exact-positive-integer?]
                                [#:timeout-ms timeout-ms (>=/c 0)]
                                [#:cache-key cache-key (or/c bytes? #f) #f])
                                path-record?]{
Called for effect. Returns a @racket[path-record] for a file.

Effect: Atomically create OR reuse a file in the @tech{workspace}.

If @racket[cache-key] is not @racket[#f], then
@racket[make-addressable-file] first attempts to find an existing
@racket[path-record] using @racket[cache-key].  If one is found,
@racket[make-addressable-file] will return that record.

In the event of a cache miss, a file is created under the context of
several safety limits. An output port @racket[to-file] is then given
bytes under the context of

@racketblock[
(transfer in to-file
  #:on-status on-status
  #:transfer-name transfer-name
  #:max-size max-size
  #:buffer-size buffer-size
  #:timeout-ms timeout-ms
  #:est-size est-size)]

In the event of any error, the created file is deleted if it exists,
and the database will not be affected. If the file is created
successfully, then the database will gain a @racket[path-record]. If
@racket[cache-key] is not @racket[#f], the database will also gain a
@racket[path-key-record].
}

@defproc[(make-addressable-directory [directory directory-exists?]) path-record?]{
Called for effect. Returns a record for a created directory.

Effect: Atomically move @racket[directory] to a path named after the
Base32 encoding of a digest. The digest is created using the
directory's own name and contents, discovered recursively. The
database will gain a record of the new directory path, which will be
returned.

@bold{Warning:} Any digests that are already computed for a path are
preferred over creating new digests from contents on disk. In the
event a file is directly modified in the @tech{workspace}, a digest
for a directory containing that file may be incorrect.
}

@defproc[(make-addressable-link [target-path-record path-record?]
                                [link-path path-string?])
                                path-record?]{
Called for effect. Returns a record for a created link.

Effect: Creates the directory path before the file name in
@racket[link-path] (if necessary), then creates a link at
@racket[link-path].  The net operation is performed non-atomically.

The target path must come from a valid @racket[path-record] because
the links may only point to files and directories created by Xiden.
}


@section{Databases}

Each @tech{workspace} may contain a @deftech{database} as a SQLite
file called @litchar{db}.  The database tracks the other contents of
a @tech{workspace}, and holds the state of any caches.


@subsection{Record Types}

This section covers the Racket structures representing parsed records
in a relation. To inspect the underlying database schema, run
@litchar{sqlite3 ./path/to/db} in your shell, then @litchar{.schema}
in SQLite's REPL.

@defstruct[record ([id (or/c exact-positive-integer? #f sql-null?)]) #:transparent]{
Represents an identifiable database record.
}

@defstruct*[(provider-record record) ([name non-empty-string?])]{
Represents a known provider. One @racket[provider-record] has many @racket[package-record]s.
}

@defstruct*[(package-record record) ([name non-empty-string?] [provider-id exact-positive-integer?])]{
Represents a known installed package. One @racket[package-record] has many @racket[edition-record]s.
}

@defstruct*[(edition-record record) ([name non-empty-string?] [package-id exact-positive-integer?])]{
Represents a known edition. One @racket[edition-record] has many
@racket[revision-record]s.
}

@defstruct*[(revision-record record) ([number revision-number?] [edition-id exact-positive-integer?])]{
Represents a known revision number under an edition. One
@racket[revision-record] has many @racket[revision-name-record]s.

@racket[number] is a @tech{revision number}, not a @racket[record-id].
@racket[edition-id] is a @racket[record-id] for an @racket[edition-record].
}

@defstruct*[(revision-name-record record) ([name non-empty-string?] [revision-id exact-positive-integer?])]{
Represents a name for a revision number.

@racket[name] need not be unique in the relation.

@racket[revision-id] is unique, and represents the @racket[record-id]
of a @racket[revision-record], @italic{not} a @tech{revision number}.
}

@defstruct[(path-record record) ([path (and/c path-string? (not/c complete-path?))]
                                 [digest bytes?]
                                 [target-id exact-positive-integer?])]{
Represents a @tech{workspace}-relative path that is expected to follow
these invariants:

@itemlist[
@item{@racket[(build-workspace-path path)] exists.}
@item{The file, directory, or link referenced by @racket[path] was created by a Xiden process.}
@item{The @racket[digest] was computed in terms of the contents of the corresponding file or directory.}
@item{If @racket[path] refers to a link, then @racket[target-id] matches the @racket[path-record] for the target of the link.}
]

A @racket[path-record] has many @racket[path-key-record]s.
}


@defstruct*[(path-key-record record) ([key bytes?] [path-id exact-positive-integer?])]{
Represents a cached path used by @racket[make-addressable-file].

@racket[key] must be unique.

@racket[path-id] is a @racket[record-id] of a @racket[path-record].
}


@defstruct*[(output-record record) ([revision-id exact-positive-integer?]
                                   [path-id exact-positive-integer?]
                                   [name non-empty-string?])]{
Represents an installed package output. @racket[output-record]s have a
1-to-1 relationship with @racket[path-record]s, and can be used to
query all other record types that identify an installed package.

@racket[path-id] refers to the @racket[path-record] for the package
output.  It must be unique.

@racket[name] refers to the name of the output. It need not be unique.
}


@subsection{High-Level Queries}

@defproc[(in-all-installed) sequence?]{
Returns a 12-value sequence representing discovery information and
file location information for all package outputs installed in the
@tech{target workspace}.

@itemlist[
@item{A @racket[record-id] for a provider name}
@item{A provider name}
@item{A @racket[record-id] for a package name}
@item{A package name}
@item{A @racket[record-id] for an edition name}
@item{An edition name}
@item{A @racket[record-id] for a revision number}
@item{A revision number}
@item{A @racket[record-id] for a package output}
@item{A package output name}
@item{A @racket[record-id] for a path}
@item{A path}
]

The values are always correct together, but do not guarentee the
existence of a path on disk. Ignoring the @racket[record-id]s, each
set of values represents a path to a named package output for a
specific revision, edition, package, and provider.
}


@defproc[(declare-link [link-path path-string?]
                       [target-record path-record?])
                       path-record?]{
Saves an existing link at @racket[link-path] as pointing to the
(presumably existing) path in @racket[target-record]. If the link
already exists in the database, the existing record is returned.

After using @racket[declare-link], the path in @racket[target-record]
becomes eligible for garbage collection when @racket[(not
(link-exists?  link-path))].
}


@defproc[(declare-output [provider-name non-empty-string?]
                         [package-name non-empty-string?]
                         [edition-name non-empty-string?]
                         [revision-number revision-number?]
                         [revision-names (listof non-empty-string?)]
                         [output-name string?]
                         [output-path-record path-record?])
                         output-record?]{
Saves the relationship between the package output named
@racket[output-name], the given discovery and version information, and
the given path record.
}

@defproc[(find-path-record [hint any/c]) (or/c path-record? #f)]{
Return a @racket[path-record] produced from searching the database
using the given hint, or @racket[#f] if no record was found.

The database query used depends on the type of @racket[hint].

@itemlist[
@item{@racket[exact-positive-integer?]: Use @racket[hint] as an expected primary key value.}
@item{@racket[path-string?]: Use @racket[hint] as an expected value for a path field in the database.}
@item{@racket[bytes?]: Use @racket[hint] as an expected value for a digest field in the database.}
@item{Anything else: Ignore hint and return @racket[#f].}
]
}

@defproc[(call-with-reused-output [query package-query-variant?]
                                  [output-name string?]
                                  [continue (-> (or/c #f exn? output-record?) any)])
                                  any]{
Returns @racket[(continue variant)], where @racket[variant] comes from
a search for an output named @racket[output-name] for a package
matching @racket[query]. The exact package used has the highest
@tech{revision number} available in the database.

If @racket[variant] is @racket[#f], then the named output is not installed.

If @racket[variant] is an @racket[output-record?], then the named
output is installed with that information.

If @racket[variant] is an @racket[exn?], then an error was encountered
in preparing or executing a SQL query.
}

@defproc[(in-xiden-objects [query package-query-variant?])
                           (sequence/c path-string?
                                       exact-positive-integer?
                                       revision-number?
                                       exact-positive-integer?
                                       path-string?)]{
Returns a 5-value @tech/reference{sequence} representing known
installed outputs of packages matching @racket[query] on the file
system.

@itemlist[
@item{An output name}
@item{A @racket[record-id] for a revision number}
@item{A @tech{revision number}}
@item{A @racket[record-id] for a path}
@item{A relative path (w.r.t. the @tech{workspace} containing the database) to a directory containing package output}
]

}


@defproc[(in-xiden-outputs [query package-query-variant?]
                           [output-name string?])
                           (sequence/c output-record?)]{
Returns a sequence of @racket[output-record] for outputs of the given
@racket[output-name], for all installed packages matching
@racket[query].
}

@defproc[(start-transaction!) (values (-> void?) (-> void?))]{
Called for effect. Returns two procedures also called for effect,
discussed below.

Effect: @racket[start-transaction!] instructs SQLite to start a
transaction against the database. If a transaction is already
underway, @racket[start-transaction!] will handle the corresponding
error from SQLite by using the existing transaction.

The first returned procedure ends the transaction, atomically
committing any changes to the database.

The second returned procedure rolls back the transaction, atomically
undoing any changes to the database.

Failure to call either procedure will leave the transaction open.  For
safety, use the returned procedures such that the program cannot help
but invoke one.
}

@defproc[(build-object-path [path-el (and/c path-string? (not/c complete-path?))] ...)
                            complete-path?]{
Equivalent to @racket[(build-workspace-path "objects" path-el ...)].
}

@defproc[(build-addressable-path [digest bytes?]) complete-path?]{
Like @racket[build-object-path], but the file name is
@racket[(encoded-file-name digest)].
}

@defproc[(in-issued-links) (sequence/c path-string? path-string?)]{
Returns a sequence of two values.

The first is a @tech{workspace}-relative path to a symbolic link
created by Xiden.

The second is a @tech{workspace}-relative path to the file referenced
by the link.
}


@section{Garbage Collection}

@defstruct*[($finished-collecting-garbage $message) ([bytes-recovered exact-nonnegative-integer?]) #:prefab]{
A @tech{message} for reporting the number of bytes freed from disk
using @racket[xiden-collect-garbage].
}

@defproc[(xiden-collect-garbage) exact-nonnegative-integer?]{
Deletes all records of links that do not actually exist on disk, then
deletes any installed files or directories in the @tech{target
workspace} with no known links.

Returns the estimated number of bytes recovered from disk. Empty
directories and links are assumed to have no size.
}
