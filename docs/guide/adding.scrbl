#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Adding and Removing Packages}

@project-name builds files within a transaction.  When you add or
remove packages, you do so within one of these transactions.

@section{Installing Packages}

To install a package, give @binary a @tech{package definition}.

@verbatim|{
$ xiden pkg -i def.rkt
}|

@litchar{-i} means “install”.

Here we used a package definition on disk, but the definition might
come from the Internet.

@verbatim|{
$ xiden pkg -i https://example.com/def.rkt
}|


@secref{queries} covers how @binary searches for a package definition
when an exact location is not clear.

@verbatim|{
$ xiden pkg -i example.com:widget:draft
}|

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
packages side-by-side, meaning if anything is different about a
package, then that package will get a unique directory. If you try to
install the same package twice, then @binary will reuse the
existing directory.


@subsection{What About a Confirmation Prompt?}

@project-name is not interactive. Its runtime configuration is held
constant after launch so that each command feels like calling a pure
function. Incorrect behavior is cause to review @secref{config}.

Your shell is already interactive and should help you abbreviate long,
repetitive commands. If calling a complete command is tedious, then
please leverage the features of your shell or configure @project-name
to find configuration values from other sources.


@section{Uninstalling Packages}

Uninstalling packages feels similar to installing. Just switch
@litchar{-i} to @litchar{-u}.

@verbatim|{
$ xiden pkg -u def.rkt -u https://example.com/def.rkt
}|

The difference is that @binary will match existing installations
against the definitions you provide. For safety, @tt{uninstall}
refuses to uninstall packages referenced by another package.


@subsection{Transaction Rules}

As before, the command defines the scope of a transaction. An
uninstallation failure will cancel out any other operations in the
transaction, including installations. So if @litchar{-u c.rkt}
fails below, @litchar{-i b.rkt} will not commit to disk even
if the installation succeeded.

@verbatim|{
$ xiden pkg -u a.rkt -i b.rkt -u c.rkt
}|


@subsection{Collecting Garbage}

Uninstallation only marks relevant files as garbage. If you later
install any packages that are already installed but marked as garbage,
then they will no longer be marked for garbage collection.

Be warned that @tech{package queries} may match more than one
installed package. Uninstalling @litchar{example.com:widget} will
target every edition, revision, and output of the named package.
@tech{Package definitions} won't have this effect because they
correspond to queries that match only themselves.

To delete all garbage packages and unreferenced files in your
@tech{workspace}, run the garbage collector. This will commit
all pending deletions in your workspace.

@verbatim|{
$ xiden gc
}|
