#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base racket/file]]

@title[#:tag "verification"]{Capturing and Restoring Workspaces}

To protect our projects from surprises, we need a way to reproduce an
exact @tech{workspace}.  Unless you keep a copy of your desired
workspace with your project, there are several sources of variability:

@itemlist[

@item{Racket bytecode files may change over time even if their runtime
behavior does not. One way this can happen is if a Racket module
embeds the current date in code.}

@item{Two users may configure @|binary| differently.}

@item{Two servers may respond with different packages for the same @tech{query}.}

]

To give you some control, @binary can capture up to all of the files
in a @tech{workspace}. With a capture, @binary can alert you to
unexpected changes and attempt to reproduce a @tech{workspace}.


@section{Capture a Workspace}

@margin-note{Why not call a capture file a lock file? To avoid confusion with the same term
in other contexts, e.g. @racket[make-lock-file-name].}  The @litchar{capture}
command creates a @deftech{capture file}.  Check this file into source control
or share it. It allows others to reproduce a workspace on their systems.

@verbatim|{
$ zcpkg capture > capture.rkt
}|

A capture file records the current @binary configuration,
@tech{queries} for all installed packages, and integrity information
for files. It does NOT follow or capture symbolic links when capturing
files.

To capture a file, that file's path must match at least one Perl
regular expression provided in your command line. If you do not
specify a pattern, the @tt{capture} command will not capture any
files.

To explicitly capture @tt{.rkt} and @tt{.rktd} files, you could
write any of the following.

@verbatim|{
$ zcpkg capture '\.rktd?$' > capture.rkt
$ zcpkg capture '\.rkt$' '\.rktd$' > capture.rkt
$ zcpkg capture '\.(rkt|rktd)$' > capture.rkt
}|

If you want to capture valid Racket compiler input only (Meaning
@tt{.rkt}, @tt{.rktd}, @tt{.ss} and @tt{.scrbl} files), then use
@litchar{-r}. That switch uses the corresponding regular expression
when creating the capture.

@verbatim|{
$ zcpkg capture -r > capture.rkt
}|

@margin-note{Be careful when you include compiler or other program
output in a capture.  Racket bytecode files may differ for source code
with no observable behavioral difference, and capturing variable data
like logs likely won't be helpful. While you could capture everything,
it's best to capture only the static data you absolutely need.}

If you wish to capture Racket bytecode as well (@tt{.dep} and
@tt{.zo}), then specify @litchar{-s}.

@verbatim|{
$ zcpkg capture -rs > capture.rkt
}|

If you want to capture all integrity information for the sake of
reproducible builds, then specify an empty pattern. This trivially
matches any path.

@verbatim|{
$ zcpkg capture '' > capture.rkt
}|

You can prepare multiple captures for different purposes.

@verbatim|{
$ zcpkg capture -r > racket-capture.rkt
$ zcpkg capture -s > racket-bytecode-capture.rkt
$ zcpkg capture '\.(html|css|js|web[pm])$' > web-asset-capture.rkt
}|


@section{Compare a Workspace to a Capture}

The @litchar{diff} command shows you any difference between a
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

Here, when I say “your workspace” I mean the directory in
@racket[ZCPKG_WORKSPACE] when you run the @litchar{diff} command.

@itemlist[
@item{@tt{+} means this file exists in your workspace, and not in the captured workspace.}
@item{@tt{-} means this file exists in the captured workspace, but not in yours.}
@item{@tt{*} means this file exists in both workspaces, but they are not the same.}
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


@section{Restore a Workspace}

The @litchar{restore} command will attempt to reproduce the workspace recorded
in a @tech{capture file}. This may take a while if the capture is
large. @litchar{restore} will not modify the filesystem unless you grant
explicit consent in your command. If you do not consent, the @litchar{restore}
command will simply show you what it would do if you did consent.

@verbatim|{
$ zcpkg restore capture.rkt
}|

Like other commands, @litchar{restore} targets @bold{an existing
workspace}. If you are restoring a capture, then @bold{it will delete
files inside that workspace}. Specifically, it will delete any file
that the @tt{diff} command reports with @litchar{+}, @litchar{*}.
This is useful for rollbacks, but the @tt{restore} command assumes
that rolling back unwanted operations entails deleting everything not
in a capture. If manually change a workspace and do not capture your
changes, then @tt{restore} will delete those changes. @italic{Be sure
to capture all changes you want to keep}.

One way to prevent surprises or noise is to use an empty
workspace. You can then move the created workspace to overwrite an
existing workspace, if you prefer.

@verbatim|{
$ ZCPKG_WORKSPACE=/tmp/ws zcpkg restore -y capture.rkt
$ ZCPKG_WORKSPACE=/tmp/ws zcpkg diff capture.rkt
$ mv /tmp/ws ../other/zcpkg-workspace
}|


@section{Example Session}

This Bash session installs a single package in a workspace, captures
that workspace, deletes the workspace outright, and then restores it.

@verbatim|{
$ zcpkg install john.doe:calculator
$ zcpkg capture -r > capture.rkt
$ zcpkg diff capture.rkt # Should show no output
$ rm -rf $(zcpkg show workspace)
$ zcpkg diff capture.rkt # Should complain about all files
$ zcpkg restore capture.rkt # Should show what will be done
$ zcpkg restore -y capture.rkt # Restore the workspace
$ zcpkg diff capture.rkt # Should show no output
}|
