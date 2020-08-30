#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Installing Packages}

To install a package, give @binary a @tech{package definition}.

@verbatim|{
$ xiden install def.rkt
}|

That definition might come from the Internet.

@verbatim|{
$ xiden install https://example.com/def.rkt
}|

@secref{queries} covers how @binary searches for a package definition
when an exact location is not clear.

@verbatim|{
$ xiden install example.com:widget:draft
}|

The commands we've used so far actually output what they @italic{would
do} if you @italic{explicitly consented} to running them. Package
managers may do more work than users intended, which is why you need
to explicitly consent when using the default settings.

@verbatim|{
$ xiden install -y def.rkt
}|

Don't worry about installing conflicting versions. @binary installs
packages side-by-side, meaning if anything is different about a
package, then that package will get a unique directory. If you try to
install the same package twice, then @binary will simply re-create
that package's directory with fresh contents.


@section{What About a Confirmation Prompt?}

@project-name is not interactive. Its runtime configuration is held
constant after launch so that each command feels like calling a pure
function. Incorrect behavior is cause to review
@secref{Configuration}.

In another view, your shell is already interactive and able to
abbreviate any long, repetitive commands. If calling a complete
command is tedious, then please leverage the features of your shell.
