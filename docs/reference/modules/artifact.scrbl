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


@defproc[(lock-artifact [arti artifact-info?]
                        [exhaust exhaust/c raise]
                        [#:source? source? any/c #t]
                        [#:integrity? integrity? any/c #t]
                        [#:signature? signature? any/c #t]
                        [#:content-budget content-budget budget/c (* 1024 50)]
                        [#:digest-budget digest-budget budget/c +inf.0]
                        [#:public-key-budget public-key-budget budget/c +inf.0]
                        [#:signature-budget signature-budget budget/c +inf.0])
                        artifact-info?]{
Returns a functionally-updated @racket[artifact-info].

When @racket[content?] is a true value, then the
@racket[artifact-info-source] field @racketid[C] is replaced by

@racketblock[
(lock-source C content-budget exhaust)
]

When @racket[integrity?] is a true value, then the
@racket[artifact-info-integrity] field @racketid[I] is replaced by

@racketblock[
(lock-integrity-info #:digest-budget digest-budget
                     I exhaust)
]

When @racket[signature?] is a true value, then the @racket[artifact-info-signature]
field @racketid[S] is replaced by

@racketblock[
(lock-signature-info #:public-key-budget public-key-budget
                     #:signature-budget signature-budget
                     S exhaust)
]
}
