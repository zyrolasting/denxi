#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/sequence
                    file/glob
                    xiden/file
                    xiden/subprogram]
          "../../shared.rkt"]

@title{Files}

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
         (subprogram/c path?)]{
Like @racket[in-paths], except this returns a @tech{subprogram} that
fails with @racket[$path-not-found] on the @tech{subprogram log} if no
paths are found.  Otherwise, the procedure uses the first matching
path.
}

@defstruct*[($path-not-found $message) ([pattern (or/c regexp? pregexp? byte-regexp? byte-pregexp? string?)] [wrt path-string?])]{
A @tech{message} reporting if @racket[(path-matching pattern wrt)] found no path.
}
