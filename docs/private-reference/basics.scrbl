#lang scribble/manual

@require["../shared.rkt"]

@title{Basic Information}

To build all project deliverables, run @litchar{make} or @litchar{make
build}. To run the tests, run @litchar{make test}. To do both, run
@litchar{make build test}.

@project-name is versioned according to @secref["versioning" #:doc '(lib
"xiden/docs/guide/xiden-guide.scrbl")] in published package definitions. Do not
follow the versioning scheme defined in @secref["Package_Concepts" #:doc '(lib
"pkg/scribblings/pkg.scrbl")], even in the @litchar{info.rkt} file.
