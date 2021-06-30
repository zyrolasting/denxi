#lang scribble/manual

@require["../shared.rkt"]

@title[#:tag "fetch-command"]{The Fetch Command}

The @litchar{fetch} command in Xiden's CLI transfers bytes from a
@tech/xiden-reference{source} expression to standard output.

@verbatim|{
$ xiden fetch '(http-source "https://example.com/file.tgz")' >file.tgz
}|

Use this to download data you see in package definitions.
@litchar{fetch} does not check the integrity or signature of the
output data.
