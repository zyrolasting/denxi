#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Managing Dependencies}

@project-name does work using transactions, such that any failure
leaves your system as it was before the transaction started.

You run a transaction using the @litchar{do} command. An installation
is one possible thing to do during a transaction.

@verbatim|{
$ xiden do -i widget def.rkt
}|

This transaction bind a symbolic link called @litchar{widget} to the
installed output of the @tech{package definition} file
@litchar{def.rkt}.

When you no longer need the built output, remove the link.

@verbatim|{
$ rm widget
}|

All files without links are eligible for garbage collection.

@verbatim|{
$ xiden gc
}|


The premise of installation and uninstallation is the same, but
@project-name reasons about them in terms of explicit transactions
and what files currently have references.


@subsection{Where Was the Confirmation Prompt?}

@project-name is not interactive. Its runtime configuration is held
constant after launch so that each command feels like calling a pure
function. Undesireable behavior is cause to review @secref{config}.

Your shell is already interactive and should help you abbreviate long,
repetitive commands. If calling a complete command is tedious, then
please leverage the features of your shell and/or configure @project-name
using sources other than a command line.


@subsection{Installing Multiple Packages}

You can specify more than one definition to install in order.  Note
that one command equals one transaction! In this context, three
installations occur in one transaction. If anything goes wrong in the
installation for @tt{example.com:widget:draft}, then the entire
transaction fails to the state before the command ran.

@verbatim|{
$ xiden pkg -i example.com:widget:draft -i def.rkt
}|

Don't worry about installing conflicting versions. @binary installs
dependencies side-by-side. If you try to install the same package
twice, then @binary will reuse the existing directory.
