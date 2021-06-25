#lang scribble/manual

@require["../../../shared.rkt"
         @for-label[racket
                    xiden/message
                    xiden/integrity
                    xiden/signature
                    xiden/signature/ffi
                    xiden/source]]

@title{Signature Checking FFI}

@defmodule[xiden/signature/ffi]

@racketmodname[xiden/signature/ffi] is a private module that defines
FFI bindings for a bundled library.


@defproc[(signature-ffi-available?!) boolean?]{
Returns @racket[#t] if the FFI dynamically linked against the bundled
foreign library for the purposes of integrity checking operations.
}


@defthing[signature-ffi-make-signature! make-signature/c]{
Returns bytes for a new signature.
See @racket[make-signature/c].
}


@defthing[signature-ffi-verify-signature! verify-signature/c]{
Returns @racket[#t] if a signature was verified by a public key.
See @racket[make-signature/c].
}


@defproc[(signature-ffi-get-find-signature-size!) any/c]{
Returns a foreign function for computing the expected size of 
a signature, or @racket[#f] if the function could not load.
}


@defproc[(signature-ffi-get-make-md-context!) any/c]{
Returns a foreign function for allocating a message digest context, or
@racket[#f] if the function could not load.
}


@defproc[(signature-ffi-get-start-signature!) any/c]{
Returns a foreign function for starting use of a cipher algorithm, or
@racket[#f] if the function could not load.

Each call must be paired with a call to the function returned from
@racket[(signature-ffi-get-end-signature!)].
}


@defproc[(signature-ffi-get-end-signature!) any/c]{
Returns a foreign function for concluding use of a cipher algorithm,
or @racket[#f] if the function could not load.
}


@defproc[(signature-ffi-get-verify-signature!) any/c]{
Returns a foreign function for verifying a signature using
library-specific data types, or @racket[#f] if the function could not
load.
}


@section{Signature Foreign Functions}

To be included.

