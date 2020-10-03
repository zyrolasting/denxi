#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title[#:tag "managing"]{Command Line Interface}

@project-name offers two entry points to its command-line interface.  One is a
launcher named @|binary|, which @tt{raco pkg} (or @|project-name|!) creates
during setup. You can also use the command line through the @litchar{raco
zcpkg} (“zero-collection package”) command if your @tt{PATH} does not include a
launcher directory kept separate from your installations's @tt{bin}.  While
@tt{raco zcpkg} and @binary both launch the same program, I will only refer to
it as @binary from now on.

@binary is a non-interactive program. Its configuration is held constant after
launch, and every one of its settings can be defined at the command line.
This way each command feels like calling a pure function.

@binary are explicit and verbose. If typing a command becomes tedious, then
please leverage the features of your shell and review the @secref{config}
section.


@section[#:tag "do"]{Running File Transactions}

@project-name writes files to your disk using transactions. This means when it
fails, your system will be left as it was before the transaction started. Run a
transaction using the @litchar{do} command.

@verbatim|{
$ xiden do ++install-source my-lib lib def.rkt
}|

Whew, that's a long command! Don't worry, we'll shorten it in the next section.
This example is the least ambiguous way to install something, so you can see
all of the relevant information.

This command defines a transaction. The transaction has only one step: To
install something from a source (@litchar{++install-source}).

@litchar{++install-source} creates a symbolic link called @litchar{my-lib}.
The link points to an output directory built using the @tech{package
definition} file @litchar{def.rkt}. That directory will hold the package's
@litchar{lib} output.

Think of the command as @italic{binding} a link to a built directory.  When you
no longer need the output, remove the link.

@verbatim|{
$ rm my-lib
}|

All files without links are eligible for garbage collection.

@verbatim|{
$ xiden gc
}|

That's it!


@subsection{Abbreviating Commands}

Some @litchar{do} command flags are verbose for flexibility reasons. You never
have to worry about file conflicts if you control the exact namespace of
dependency references, and you never have to worry about bloat if you control
what exact deliverable each package places on your disk.

Even so, not everyone wants to type all options all of the time. Different
flags offer different levels of abbreviation for instructions.  Run
@litchar{xiden do -h} to review your options, but note that all of these
commands are equivalent (assuming that the name of the package in @tt{def.rkt}
is @racket{widget}):

@verbatim|{
# long flags
$ xiden do ++install-source widget default def.rkt
$ xiden do ++install-default widget def.rkt
$ xiden do ++install-abbreviated def.rkt

# short flags
$ xiden do +s widget default def.rkt
$ xiden do +d widget def.rkt
$ xiden do +a def.rkt
}|

If you want to use even shorter commands, then you can leverage your shell.

@verbatim|{
$ xi() { xiden do +a $1 }
$ xi def.rkt
}|


@subsection{Installing Multiple Packages}

You can specify more than one definition to install in order.  Note
that one @litchar{do} command equals one transaction! In this context,
three installations occur in one transaction. If anything goes wrong
in the third installation, then the entire transaction fails to the
state before the command ran.

@verbatim|{
$ xiden do ++install-source ... ++install-source ... ++install-source ...
}|

Don't worry about installing conflicting versions. @binary installs
dependencies side-by-side. If you try to install the same package
twice, then @binary will reuse the existing directory.


@subsection{Creating Arbitrary Links}

Sometimes it makes sense to make links to specific files in a package's
output. A good way to do this is to create a link as normal using a relative
path that follows a link you've already created using @litchar{xiden do}.

@verbatim|{
$ xiden do +d vendor definition.rkt
$ ln -s vendor/widget/main.rkt widget.rkt
}|

The link created using your operating system is not tracked by @|project-name|,
so a garbage collection pass can break the link. But when you use a relative
path as shown, then you can repair the link by running the same transaction.


@section{Printing Reports}

Use the @litchar{show} command to review key information.

@subsection{View Path to Workspace}

@litchar{xiden show workspace} shows the path to the @tech{target
workspace}. See @secref{workspace} for more information on how @project-name
defines this path.

@subsection{View Installed Outputs}

@litchar{xiden show installed} shows all installed outputs.  Each line contains
a @tech{package query} matching an exact @tech{package definition} used on your
system, a name for an output used with that definition, and the path where the
output is located in the @tech{target workspace}.

@subsection{View Issued Links}

@litchar{xiden show links} shows all records of symbolic links issued by
@|project-name| for the @tech{target workspace}. Each line is formatted as
@litchar{L -> T}, where @litchar{L} is a path to a symbolic link, and
@litchar{T} is a path to the linked file on disk.

Either path might be a relative path. If it is, then that path is relative to
the @tech{target workspace}.

Note that @litchar{L} might not exist. @litchar{xiden gc} will remove any link
record where the link does not exist at @litchar{L}.


@section[#:tag "gc"]{Collecting Garbage}

We briefly visited the @litchar{gc} command in @secref{do}. We observed that if
you delete a link issued by @litchar{xiden do} on your disk, then
@litchar{xiden gc} may collect the target.

A file or directory is eligible for garbage collection if it has no incoming
links issued by @|project-name|.

The garbage collector follows a three step process:

@itemlist[#:style 'ordered
@item{Forget about any links where a link file does not actually exist on disk.}
@item{Delete all files or directories in the @tech{target workspace}'s @litchar{var/xiden/objects} directory with no incoming links.}
@item{If nothing was actually deleted in Step 1 or Step 2, print the number of bytes recovered. Otherwise, go to Step 1.}
]

Note that only the immediate contents (as in @racket[directory-list]) of a
@tech{workspace}'s @litchar{var/xiden/objects} directory are monitored, because
@project-name @italic{does not issue links to the contents of any
subdirectory}. This means that if you ever create a symbolic link to a file
inside of a package's output directory, then that link is dependent on the
output directory remaining after any garbage collection pass.


@section{Managing Runtime Configuration}

Use the @litchar{xiden config} command to manage @|binary|'s configuration.

I'll use the @tt{XIDEN_VERBOSE} setting for the below examples:

@itemlist[
@item{@litchar{xiden config dump}: Prints a readable hash of the active configuration.}
@item{@litchar{xiden config set XIDEN_VERBOSE "#t"}: Changes a setting in a @tech{workspace}'s @tt{etc/xiden.rkt} file.}
@item{@litchar{xiden config get XIDEN_VERBOSE}: Gets the value of a setting}
]
