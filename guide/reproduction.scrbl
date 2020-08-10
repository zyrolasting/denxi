#lang scribble/manual

@require["shared.rkt" @for-label[racket/base]]

@title[#:tag "verification"]{Reproducing Work}

To keep things relatively sane, we need reproducible and deterministic builds.
This means that for the same input, we get the exact same files.

Unfortunately, this is a high bar to reach unless you keep a copy of all
dependencies with your project. Racket bytecode files may differ even if they
behave the same when executed, simply because they embed a changing date in
their code. A @tech{workspace} might also contain different configurations. On
top of that, @tech{revision names}, being provider-asserted, can refer to
different revision numbers on different servers. @binary includes tools to
capture files in @tech{workspace}, such that only the files you declare
are not expected to change.

@margin-note{Why not call it a lock file? To avoid confusion with the same term
in other contexts, e.g. @racket[make-lock-file-name].}  The @litchar{capture}
command creates a @deftech{capture file}.  Check this file into source control
or share it. It allows others to reproduce a workspace on their systems.

@verbatim|{
$ zcpkg capture > capture.rkt
}|

A capture file records the current @binary configuration, all installed
packages, and integrity information for files. It does NOT follow or capture
symbolic links. To capture a file, that file's path must match at least one
Perl regular expression provided in your command line. If you do not specify a
pattern, the @tt{capture} command will only record integrity information for
@tt{.rkt}, @tt{.rktd}, @tt{.ss}, @tt{.scrbl}, or @tt{.ss} files.

To explicitly capture @tt{.rkt} and @tt{.rktd} files, you could
write either of the following.

@verbatim|{
$ zcpkg capture '\.rktd?$' > capture.rkt
$ zcpkg capture '\.rkt$' '\.rktd$' > capture.rkt
}|

If you want to capture all integrity information for the sake of
reproducible builds, then specify an empty pattern. This trivially
matches any path.

@verbatim|{
$ zcpkg capture '' > capture.rkt
}|

Be careful in this case.  Racket bytecode files may differ beetween builds if,
say, a program embeds dates into generated code.

The @litchar{restore} command will attempt to reproduce the workspace recorded
in a @tech{capture file}. This may take a while if the capture is
large. @litchar{restore} will not modify the filesystem unless you grant
explicit consent in your command. If you do not consent, the @litchar{restore}
command will simply show you what it would do if you did consent.

The last command a restore operation performs is @litchar{zcpkg diff}, which
you can also run yourself. @litchar{diff} shows you any difference between a
@tech{workspace} and a capture file.

The output might look like this:

@verbatim|{
$ zcpkg diff capture.rkt
- usr/lib/racket/localhost.localdomain/bar/draft/4/setup.rkt
* usr/lib/racket/localhost.localdomain/baz/draft/3/setup.rkt
+ usr/lib/racket/localhost.localdomain/foo/draft/0/zcpkg-deps/localhost.localdomain/bar/great.rkt
* usr/lib/racket/localhost.localdomain/foo/draft/0/zcpkg-deps/localhost.localdomain/baz/setup.rkt
* usr/lib/racket/localhost.localdomain/baz/draft/cool/setup.rkt
- usr/lib/racket/localhost.localdomain/foo/draft/0/mod.rkt
- usr/lib/racket/localhost.localdomain/foo/draft/0/zcpkg-deps/localhost.localdomain/bar/setup.rkt
+ usr/lib/racket/localhost.localdomain/bar/draft/4/great.rkt
+ usr/lib/racket/localhost.localdomain/foo/draft/0/fo.rkt
}|

@itemlist[
@item{@tt{+} means this file exists in your workspace, and not in the captured workspace.}
@item{@tt{-} means this file exists in the captured workspace, but not in yours.}
@item{@tt{*} means this file exists in both workspaces, but has different content.}
]

The paths contain redundant information. For example, these two paths
reference the same file.

@verbatim|{
+ usr/lib/racket/localhost.localdomain/foo/draft/0/zcpkg-deps/localhost.localdomain/bar/great.rkt
+ usr/lib/racket/localhost.localdomain/bar/draft/4/great.rkt
}|

This is because the former path uses a symbolic link to reference a
dependency.  They are both included in the output as a visual clue to
what installed packages are affected by discrepencies.

If the @litchar{diff} command has no output and a zero exit code,
then your workspace's files have the same content as recorded
in a given capture file.

Like other commands, @litchar{restore} targets @bold{an existing
workspace}. If you are restoring a capture, then it will delete files
inside that workspace. This is useful for rollbacks.

If you want to limit noise when verifying results or avoid deletions,
use an empty workspace. You can then move the created workspace to
overwrite an existing workspace, if you prefer.

@verbatim|{
$ mkdir zcpkg-workspace
$ zcpkg restore -y capture.rkt
$ zcpkg diff capture.rkt
$ mv zcpkg-workspace ../other/zcpkg-workspace
}|
