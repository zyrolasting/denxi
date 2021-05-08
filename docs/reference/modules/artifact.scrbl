#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/path
                    xiden/subprogram
                    xiden/artifact
                    xiden/integrity
                    xiden/monad
                    xiden/signature
                    xiden/source
                    xiden/string]
         "../../shared.rkt"]

@title{Artifacts}

@defmodule[xiden/artifact]

@defstruct*[artifact-info ([source source-variant?]
                           [integrity (or/c #f well-formed-integrity-info/c)]
                           [signature (or/c #f well-formed-signature-info/c)])]{
An @deftech{artifact} is an instance of @racket[artifact-info].  Each
instance provides a @tech{source} and the means to verify the bytes
produced when the source is @tech{tapped}.
}

@defproc[(artifact [source source-variant?]
                   [int (or/c #f well-formed-integrity-info/c) #f]
                   [sig (or/c #f well-formed-signature-info/c) #f])
                   artifact-info?]{
An abbreviated constructor for @racket[artifact-info].
}


@defproc[(verify-artifact [arti artifact-info?] [pathrec path-record?])
         (subprogram/c void?)]{
Returns a @tech{subprogram} that fails in the event an
@tech{artifact} does not meet the restrictions set by the
@tech{runtime configuration}.
}

@defproc[(fetch-artifact [name string?] [arti artifact-info?])
         (subprogram/c path-record?)]{
Like @racket[subprogram-fetch], but the content is expected to be an
@tech{artifact}.
}

