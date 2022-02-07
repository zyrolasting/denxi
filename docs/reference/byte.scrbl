#lang denxi/document

@title{Byte Strings}

@defmodule[denxi/byte]

@racketmodname[denxi/byte] provides operations for Racket byte
strings.

@defproc[(patch-bytes [proposed-dest bytes?]
                      [dest-start exact-nonnegative-integer?]
		      [patch bytes?]
		      [buffer-length exact-positive-integer? 4096]
		      [padding-byte byte?])
		      bytes?]{
Returns a mutable byte string @racket[dest] that contains the entirety
of @racket[patch] starting at @racket[dest-start].

@racketblock[
(bytes=? patch
         (subbytes dest
	           dest-start
		   (+ dest-start
		      (bytes-length patch))))
]

If @racket[proposed-dest] is mutable and large enough to hold
@racket[patch] without reallocation, then @racket[patch-bytes] returns
@racket[proposed-dest]. Note that this is the only case where
@racket[proposed-dest] is not copied internally.

@racket[proposed-dest] is any byte string suggested as a destination
for the patch. The first byte of @racket[patch] appears at
@racket[dest-start] position in the returned bytes.

If @racket[proposed-dest] is too small to store @racket[patch]
starting at @racket[dest-start], then @racket[patch-bytes] will
allocate a new mutable byte string. The new byte string starts with
@racket[proposed-dest], and ends with enough @racket[padding-byte]s to
create the smallest length needed to hold @racket[patch], and remain
divisible by @racket[buffer-length].
}


@section{Grannies}

A @deftech{granny} is a stereotype of a grandmother who stitches
quilts out of love for her family. Each metaphorical patch on the
quilt is equivalent to a use of @racket[patch-bytes].

Use a granny to asynchronously apply patches to mutable byte strings
encapsulated by the granny. A @deftech{quilt} is any byte string
produced by a granny.

A granny may be used as a @tech/reference{synchronizable event}, where
the @tech/reference{synchronization result} is a byte string
reflecting the accumulated result of calling @racket[patch-bytes].
One result will be available for every call to @racket[give-patch],
but not every result reflects the result of the corresponding call to
@racket[give-patch]. Programs aiming to capture every update to a
quilt must synchronize with the granny immediately after every call to
@racket[give-patch]. In the event the quilt is mutated without
reallocation at least twice, at least two @racket[eq?]
@tech/reference{synchronization results} will appear, pointing to the
latest state of the quilt.

@defproc[(make-granny [initial-patch bytes?]) granny]{
Return a @tech{granny} as an opaque value.

@racket[initial-patch] is a byte string used to derive subsequent
calls to @racket[patch-bytes]. The buffer size used for
@racket[patch-bytes] is @racket[(bytes-length initial-patch)].

@racket[(granny-stiching? (make-granny initial-patch))] is @racket[#t]
for any value of @racket[initial-patch].
}


@defproc[(granny-stitching? [g granny]) boolean?]{
Returns @racket[#t] if the @tech{granny} will respond to @racket[give-patch].
}

@defproc[(granny-quilt-ready? [g granny]) boolean?]{
Returns @racket[#t] if the @tech{granny} applied as many patches as
she received.
}

@defproc[(take-quilt [g granny] (and/c bytes? immutable?))]{
Returns a @tech{quilt} from the @tech{granny}.

Effect: @racket[(granny-stiching? g)] is henceforth @racket[#f].

To produce a new quilt, use a new @tech{granny}.
}

@defproc[(give-patch [granny granny-stiching?]
                     [patch bytes?]
		     [dest-start exact-nonnegative-integer?])
		     granny)]{
Queues a call to @racket[patch-bytes] with respect to a @tech{granny}.

Each invocation will asynchronously mutate or replace a @tech{quilt}.
}
