#lang scribble/manual

@require["../shared.rkt"]

@title[#:tag "show-command"]{The Show Command}

The @litchar{show} command allows you to inspect Xiden's state.

@define[target-workspace @tech/xiden-reference["target workspace"]]

@litchar{xiden show installed} shows everything installed in a
@|target-workspace|. A workspace is a directory on your disk where
Xiden stores its state.

Each line in the output of @litchar{show installed} contains a
@tech{package query}, an output name, and a path where the output is
located.

@litchar{xiden show links} shows all records of symbolic links issued
by Xiden for the @|target-workspace|. Each line is formatted as
@litchar{L -> T}, where @litchar{L} is a path to a symbolic link, and
@litchar{T} is a path to the linked file on disk.  Either path might
be a relative path. If it is, then that path is relative to a
@|target-workspace|. Note that @litchar{L} might not exist. The
@litchar{gc} command will remove any link record where the link does
not exist at @litchar{L}.
