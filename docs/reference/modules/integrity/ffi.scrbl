#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    xiden/integrity/ffi]
         "../../../shared.rkt"]

@title{Integrity FFI}

@defmodule[xiden/integrity/ffi]

@racketmodname[xiden/integrity/ffi] is a private module that defines
FFI bindings for a bundled library dedicated to integrity checking.

@defproc[(integrity-ffi-available?!) boolean?]{
Returns @racket[#t] if the FFI dynamically linked against the bundled
foreign library for the purposes of integrity checking operations.
}

@defthing[integrity-ffi-chf-available?! predicate/c]{
Returns @racket[#t] if the input argument is a symbol, and can be used
to load a CHF implemented in the foreign library.
}

@defproc[(integrity-ffi-get-c-chfs!) (or/c #f (listof symbol?))]{
Returns a list of symbols that suitable for use with other functions
that accept symbols as inputs.
}

@defproc[(integrity-ffi-get-default-chf-index!) (or/c #f exact-nonnegative-integer?)]{
Returns a foreign value representing the index of
@racket[(integrity-ffi-get-supported-chfs!)] suggested for use as a
default CHF.

Do not confuse this index with @racket[get-default-chf].  The default
is a suggestion by the library among its own implementations, and does
not apply as a default for the runtime unless it is installed using
@racket[current-chfs].
}

@defproc[(integrity-ffi-get-load-chf!) any/c]{
Returns a foreign function for loading a CHF implementation in the C
runtime. In OpenSSL, the C runtime uses the default implementation
provider.
}

@defproc[(integrity-ffi-get-get-digest-size!) any/c]{
Returns a foreign function used to report the expected size of a
message digest from a given CHF implementation.
}

@defproc[(integrity-ffi-get-make-digest!) any/c]{
Returns a foreign function used to create a message digest using a CHF
implementation.
}

@defproc[(integrity-ffi-get-chf-count!) (or/c #f exact-positive-integer?)]{
Returns a Racket positive integer, or @racket[#f] on load failure.

The integer represents the number of CHFs supported by the C runtime,
not the number of implementations available.
}

@defproc[(integrity-ffi-get-supported-chfs!) any/c]{
Returns a FFI value as a C string array. Each element represents the
name of a CHF. There are @racket[(integrity-ffi-get-chf-count!)]
elements in the array.
}


@section{Integrity Foreign Functions}

To be included.

