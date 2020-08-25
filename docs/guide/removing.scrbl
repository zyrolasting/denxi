#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Uninstalling Packages}

Uninstalling a package feels similar to installing a package,
in that the command accepts the same kind of arguments.

@verbatim|{
$ xiden uninstall def.rkt
$ xiden uninstall https://example.com/def.rkt
$ xiden uninstall example.com:widget:draft
}|

The difference is that @binary will find matching installed packages
based on the information you provide. @binary will tell you what it
will remove. As before, explicit consent actually uninstalls the
packages.

@verbatim|{
$ xiden uninstall -y def.rkt
}|
