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

Here's an example that uses @|binary|'s own query format, which can
match definitions served from a configured list of endpoints. See
@secref{queries} for more information.

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
