#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    xiden/message
                    xiden/integrity
                    xiden/input-info]
         "../shared.rkt"]

@title{Resolving Inputs}

@defmodule[xiden/input-info]

A @deftech{package input} is an instance of @racket[input-info].

@defstruct*[input-info ([name string?] [sources (non-empty-listof string?)] [integrity (or/c #f integrity-info?)] [signature (or/c #f signature-info?)]) #:prefab]{
A structure representing a request for exact bytes.
}


@defproc[(input [name string?]
                [sources (non-empty-listof string?)]
                [integrity (or/c #f well-formed-integrity-info/c) #f]
                [signature (or/c #f well-formed-signature-info/c) #f])
         well-formed-input-info/c]{
An abbreviated @racket[input-info] constructor that performs stronger validation on its arguments.
}


@defproc[(input-ref [inputs (listof input-info?)] [name string?]) (or/c #f input-info?)]{
Returns the first element of @racket[inputs] @racket[I] where
@racket[(input-info-name I)] is @racket[name], or @racket[#f] if no
such element exists.
}


@defproc[(call-with-input [input input-info?] [proc (-> path-string? any)]) any]{
Returns @racket[(proc P)], where @racket[P] is a path to a symbolic link. That link
points to the file produced using @racket[(resolve-input input)].

@racket[call-with-input] deletes @racket[P] after @racket[proc]
finishes, and before returning control to the caller.

Use for creating references to dependencies that only apply during a
build. This allows an input's target file to remain eligible for
garbage collection.
}


@defproc[(keep-input! [input input-info?]) path-string?]{
Returns a path to a symbolic link derived from @racket[input].
The link is created as a side effect, and will not be deleted
by @racket[keep-input!].

Use for creating references to dependencies that persist after
a build. Any file referenced in this way will not be eligible
for garbage collection unless all links are removed.
}


@defproc[(resolve-input [input input-info?]) logged?]{
Returns a deferred computation that, when applied, acquires and
verifies the bytes for @racket[input].

The process will fail if the bytes do not meet the requirements
of @racket[input], if no bytes are available, or if the runtime
configuration does not place trust in the bytes.
}