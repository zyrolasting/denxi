#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Installing Packages}

To install a package, give @binary a @tech{package definition}.

@verbatim|{
$ xiden install def.rkt
}|

That definition might come from the Internet.

@verbatim|{
$ xiden install https://example.com/...
}|

@binary also comes with a plugin system, where you can provide custom
queries that resolve to definitions. Here's an example that uses
@|binary|'s own query format.

@verbatim|{
$ xiden install example.com:widget
}|

@binary is hypervigilant. The commands we've used so far actually say
what they @italic{would do} if you @italic{explicitly consented} to
running them (Package managers may do more work than users intended).

@verbatim|{
$ xiden install -y def.rkt
}|

Don't worry about installing conflicting versions. @binary installs
packages side-by-side, meaning if anything is different about a
package, then that package will get a unique directory. If you try to
install the same package twice, then @binary will simply re-create
that package's directory with fresh contents.
