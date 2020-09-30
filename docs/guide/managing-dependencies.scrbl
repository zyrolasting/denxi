#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title[#:tag "managing"]{Managing Dependencies}

@project-name installs files in transactions. This means when it
fails, your system will be left as it was before the transaction
started. Run a transaction using the @litchar{do} command.

@verbatim|{
$ xiden do ++install-source my-lib lib def.rkt
}|

This transaction creates a symbolic link called @litchar{my-lib}.
The link points to an output directory built using the @tech{package
definition} file @litchar{def.rkt}. That directory will hold
the package's @litchar{lib} output.

Think of the command as @italic{binding} a link to a directory.  When
you no longer need the output, remove the link.

@verbatim|{
$ rm my-lib
}|

All files without links are eligible for garbage collection.

@verbatim|{
$ xiden gc
}|

That's it.

@section{Well, wait a minute. I have to specify three things to install one thing?}

Not always. There are other command line flags that require less
information. Run @litchar{xiden do -h} to see what's available.

The earlier example showed you the most verbose action because it creates
contrast against a more semantic @litchar{install} command.

@verbatim|{
$ xiden install def.rkt
}|

While easier on the eyes, doing it this way would introduce a lot of hidden
behavior. It would force @binary to make up answers to questions like “Where do
I create a launcher?” or “What do I need to build?” If a package author or tool
makes those decisions for you, then that is convenient until you need to do
things differently. Words like @litchar{install} hide too much, precisely
because you will eventually want more control over the process.

Flags used with the @litchar{do} command can be verbose because that is the
cost of flexibility. You never have to worry about file conflicts if you
control the exact namespace of dependency references, and you never have to
worry about bloat if you control what exact deliverable each package places on
your disk.


@section{Any Confirmation Prompts?}

@project-name is not interactive. Its runtime configuration is held
constant after launch so that each command feels like calling a pure
function. Undesireable behavior is cause to review @secref{config}.

Your shell is already interactive and should help you abbreviate long,
repetitive commands. If calling a complete command is tedious, then
please leverage the features of your shell and/or configure @project-name
using sources other than a command line.


@section{Installing Multiple Packages}

You can specify more than one definition to install in order.  Note
that one @litchar{do} command equals one transaction! In this context,
three installations occur in one transaction. If anything goes wrong
in the third installation, then the entire transaction fails to the
state before the command ran.

@verbatim|{
$ xiden pkg ++install-source ... ++install-source ... ++install-source ...
}|

Don't worry about installing conflicting versions. @binary installs
dependencies side-by-side. If you try to install the same package
twice, then @binary will reuse the existing directory.
