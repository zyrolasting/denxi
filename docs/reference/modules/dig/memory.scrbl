#lang scribble/manual

@require[@for-label[racket
                    xiden/artifact
                    xiden/logged
                    xiden/dig
                    xiden/dig/memory
                    xiden/artifact
                    xiden/query
                    xiden/source]]


@title{Memory Shovels}

@defmodule[xiden/dig/memory]

@defproc[(make-memory-shovel [contents (hash/c any/c artifact-info?)]) shovel/c]{
Returns a @tech{shovel} that behaves similarly to @racket[(curry
hash-ref contents)]. If the returned procedure cannot find an artifact
in @racket[contents] for some @racketid[key], then it will behave like
@racket[(broken-shovel key)].

@racketblock[
(define dig (make-memory-shovel (hash 'a (artifact (text-source "hello")))))

(dig 'a) (code:comment "OK")
(dig 'b) (code:comment "Fails")
]
}

@defproc[(make-memory-shovel/pkgdef [contents hash?]
                                    [defaults package-query-defaults-implementation/c])
                                    dig/c]{
Returns a @tech{shovel} that searches @racket[contents] for artifacts
using @tech{package queries}.

@racket[contents] is a nested hash table keyed first by provider
names, then package names, then edition names, then both revision
names and numbers as shown.

@racketblock[
(define providers contents)
(define packages (hash-ref providers "example.com"))
(define editions (hash-ref packages "rpg"))
(define revisions (hash-ref editions "directors-cut"))
(artifact-info? (hash-ref revisions 0))
(artifact-info? (hash-ref revisions 28))
(artifact-info? (hash-ref revisions (hash-ref revisions "re-release")))
]

As indicated by the last line, each hash table may map keys to other
keys in the same table.

e.g.

@racketblock[
(hash-ref providers (hash-ref providers "alias.example.com"))
]

The shovel will recursively resolve keys in this way until it
encounters a value of a type not used as a key, in observance of the
digging metaphor. If the search yields cyclic keys, this function will
not terminate.

Package queries are autocompleted using @racket[defaults], and
converted to canonical form with respect to a canon based on the hash
table's shape.

Example:

@racketblock[
(define dig
  (make-memory-shovel/pkgdef
    (hash "default" "jon"
          "jon" (hash "calculator"
                      (hash "scientific"
                            (hash 0 (artifact #"...")
                                  8 (artifact #"...")
                                  "initial" 0
                                  "beta" 8))))))

(dig ":calculator:scientific:initial:beta")
]
}
