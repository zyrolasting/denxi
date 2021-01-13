#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/path
                    xiden/logged
                    xiden/message
                    xiden/integrity
                    xiden/input-info
                    xiden/signature
                    xiden/source
                    xiden/string]
         "../../shared.rkt"]

@title{Resolving Inputs}

@defmodule[xiden/input-info]

A @deftech{package input} is an instance of @racket[input-info].

@defstruct*[input-info ([name string?] [source source?] [integrity (or/c #f integrity-info?)] [signature (or/c #f signature-info?)]) #:prefab]{
A structure representing a request for exact bytes from a @tech{source}.
}


@defproc[(make-input-info [name string?]
                          [source source? null]
                          [integrity (or/c #f well-formed-integrity-info/c) #f]
                          [signature (or/c #f well-formed-signature-info/c) #f])
         well-formed-input-info/c]{
A contracted @racket[input-info] constructor.
}


@defthing[well-formed-input-info/c
          flat-contract?
          #:value (struct/c input-info
                            file-name-string?
                            (non-empty-listof string?)
                            (or/c #f well-formed-integrity-info/c)
                            (or/c #f well-formed-signature-info/c))]{
A contract that recognizes @tech{package inputs} suitable for use in
@tech{package definitions}.
}

@defthing[abstract-input-info/c
          flat-contract?
          #:value (struct/c input-info file-name-string? #f #f #f)]{
A contract that recognizes @tech{package inputs} with only a name defined.
}

@defthing[concrete-input-info/c
          flat-contract?
          #:value (and/c well-formed-input-info/c (not/c abstract-input-info/c))]{
A contract that recognizes @tech{package inputs} with a name and a source.
}

@defthing[current-inputs (parameter/c (listof input-info?))]{
A @tech/reference{parameter} bound to inputs to use with @racket[input-ref].
}

@defproc[(input-ref [name string?]) (logged/c input-info?)]{
Returns the first element of @racket[(current-inputs)] with an
@racket[input-info-name] @racket[equal?] to @racket[name].
}

@defproc[(find-input [inputs (listof input-info?)] [name string?]) (logged/c input-info?)]{
Returns a @tech{logged procedure} @racketid[P] that either
uses the first input in @racket[inputs] with the given name,
or fails with @racket[$input-not-found] on the program log.
}

@defproc[(release-input [input input-info?]) (logged/c void?)]{
Returns a @tech{logged procedure} @racketid[P] that deletes the
symbolic link derived from @racket[input].
}

@defproc[(resolve-input [input input-info?]) (logged/c path-string?)]{
Returns a @tech{logged procedure} @racketid[P] that, when applied,
acquires and verifies the bytes for @racket[input]. @racketid[P]
returns a relative path to a symbolic link in
@racket[(current-directory)].

The process will fail if the bytes do not meet the requirements
of @racket[input], if no bytes are available, or if the runtime
configuration does not place trust in the bytes.
}

@defstruct*[($input $message) ([name string?])]{
A @tech{message} regarding an input with a given name.
}

@defstruct*[($input:not-found $input) ()]{
Represents a failure to find an input using @racket[find-input].
}

@defstruct*[($input:fetch $input) ([errors (listof $message?)])]{
Represents the status for fetching an input from a source.

If @racket[errors] is empty, then the fetch was successful.
Otherwise, the fetch failed, and @racket[errors] holds @tech{messages}
that explain why.
}
