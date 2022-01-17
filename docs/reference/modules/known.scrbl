#lang scribble/manual

@require[@for-label[racket
                    denxi/known]
         "../../shared.rkt"]

@title{Knowns}

A @deftech{known} is an implementation of the @racket[gen:known]
@tech/reference{generic interface}. The interface provides a method
for addressing and replacing byte content using one of several names.

@section{Known Generic Interface}

@defthing[gen:known]{
Defines a @tech/reference{generic interface} for @tech{knowns}.

Knowns encapsulate a byte string that may or may not be external to
the process. The byte string has a canonical name, and aliases for the
canonical name.

The byte content can only be replaced in full using open and close
semantics. The implementation controls synchronization and error
handling in that regard.

Names may use any type, so long as the values are distinct.  The first
name given to a known is canonical with respect to that known. All
subsequent names are aliases for the first. Names may change
independently of byte content.

Each known shares an implicit namespace for canonical names. Aliases
use a separate namespace. e.g. @racket{Sean Connory} may be an alias
for the known canonically named @racket{007}.  @racket{Sean Connory}
may also be a canonical name for a known representing the actor
himself. The two occurences of @racket{Sean Connory} do not collide in
this scenario.
}

@defthing[known? predicate/c]{
Returns @racket[#t] when given a partial or full implementation of
@racket[gen:known].
}

@defproc[(known-by [k known?]) (subprogram/c stream?)]{
Returns a @tech{subprogram} for producing a stream of names.

The stream may be empty, in which case @racket[k] is anonymous. See
@racket[known-here?].

The stream's first element is the canonical name for @racket[k].  All
remaining elements are aliases to the canonical name.  Aliases may
appear in any order.
}

@defproc[(known-open-output [k known?])
         (subprogram/c (case-> (-> input-port? transfer-policy? void?)
	                       (-> void?)))]{
Returns a @tech{subprogram} for opening the byte content of a @tech{known}
for writing. Prior output from @racket[(known-open-input k)] should be
considered invalid after applying @racket[(known-open-output k)], and
before the subprogram finishes without error.

The subprogram computes a @racket[write-or-close] procedure.

@racket[(write-or-close in)] consumes all bytes from @racket[in] to
append to existing bytes, starting from the empty byte string.


@racket[(write-or-close)] applies close semantics, such that
subsequent evaluations of @racket[(write-or-close in)] have an
implementation-defined behavior other than consuming bytes from
@racket[in].
}



@defproc[(known-open-input [k known?])
         (subprogram/c (case-> (-> output-port? transfer-policy? void?)
	                       (-> void?)))]{
Returns a @tech{subprogram} for opening the byte content of a known
for reading. The program must produce the same bytes provided in a
prior application of @racket[known-open-output], starting from
the first byte written.

If the known has no byte content, then the implementation may
immediately provide @racket[eof] instead of failing.

The subprogram computes a @racket[read-or-close] procedure.

@racket[(read-or-close out policy)] directs all existing bytes to
@racket[out]

@racket[(read-or-close)] applies close semantics, such that subsequent
evaluations of @racket[(read-or-close out)] have an
implementation-defined behavior other than writing bytes to
@racket[out].
}


@section{Derived Known Procedures}

These procedures apply derived logic to @tech{knowns}.

@defproc[(known-elsewhere? [k known?]) boolean?]{
Returns @racket[#t] if @racket[k] has at least one alias.
}

@defproc[(known-here? [k known?]) boolean?]{
Returns @racket[#t] if @racket[k] has a canonical name.
}

@defproc[(replace-known-bytes [k known?]
			      [write-all-bytes
			       (-> output-port?
			           (subprogram/c exact-nonnegative-integer?))]
			(subprogram/c exact-nonnegative-integer?)]{
Returns a @tech{subprogram} for replacing the byte content
encapsulated by @racket[k].

@racket[write-all-bytes] accepts an output port and returns another
subprogram for immediate execution. That subprogram must return the
number of bytes written, and log any errors.

When control leaves @racket[write-all-bytes] for any reason, the
output port is flushed and closed.
}
