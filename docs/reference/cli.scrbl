#lang scribble/manual

@require[@for-label[racket/base racket/cmdline]
         "../shared.rkt"]

@title{Command Line Interface}

Xiden's command line interface emphasizes full control over
the @tech{runtime configuration} at the cost of short commands. All
commands generate exact program definitions, such that unit tests
can verify the full behavior of a Xiden process.

@bold{This API is private, and may change.} To understand this
section, you must understand @racket[parse-command-line].

@include-section{modules/cli-flag.scrbl}
@include-section{modules/cmdline.scrbl}
@include-section{modules/cli.scrbl}
