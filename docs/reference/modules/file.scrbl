#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/sequence
                    file/glob
                    xiden/file
                    xiden/logged]
          "../../shared.rkt"]

@title{File System}

@defmodule[xiden/file]

@racketmodname[xiden/file] extends and reprovides
@racketmodname[racket/file].

@defproc[(in-paths [pattern (or/c regexp? pregexp? byte-regexp? byte-pregexp? string?)]
                   [start directory-exists? (current-directory)])
         (sequence/c path?)]{
Returns a sequence of paths relative to @racket[start] that match
@racket[pattern].

If @racket[pattern] is a string and not a regular expression object,
then @racket[pattern] is used as a glob pattern for use in
@racket[glob-match?].
}

@defproc[(path-matching [pattern (or/c regexp? pregexp? byte-regexp? byte-pregexp? string?)]
                        [start directory-exists? (current-directory)])
         (logged/c path?)]{
Like @racket[in-paths], except this returns a @tech{logged procedure}
that fails with @racket[$no-matching-paths] on the program log if no
paths are found.  Otherwise, the procedure uses the first matching
path.
}
