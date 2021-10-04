#lang scribble/manual

@title{Cryptography}

@require[@for-label[racket/base
                    racket/contract
                    racket/pretty
                    @rename-in[ffi/unsafe (-> -->)]
                    denxi/crypto]
         racket/pretty
         denxi/crypto
         "../../shared.rkt"]

@defmodule[denxi/crypto]

@racketmodname[denxi/crypto] interacts with a bundled derivative of an
OpenSSL library. This module has no side-effects on instantiation, but
will mutate either a FFI object in the Racket runtime, or an error
code cache in the C runtime. To reset the Racket runtime cache, call
@racket[crypto-clear-ffi-cache!]. To dump the error code cache, call
@racket[crypto-dump-error-queue!].

@defproc[(crypto-clear-ffi-cache!) void?]{
Clears an internal cache for the low-level library and its
objects. The cache operates independently of accumulated errors.
}

@defproc[(crypto-get-lib!) (or/c ffi-lib? exn?)]{
Returns a @tech/foreign{foreign-library value} for the bundled
cryptographic library, or an exception explaining why it failed to
load.

The value returned from this function is cached in Racket-managed
memory. If the cached value is an exception, then
@racket[(get-crypto-lib!)] will attempt to load the library
again. This will replace the cached exception.
}


@defproc[(crypto-get-obj! [objname (or/c bytes? symbol? string?)]
                          [type ctype?])
                          any/c]{
Behaves like @racket[get-ffi-obj] when @racket[(get-crypto-lib!)] is
a @tech/foreign{foreign-library value}.

Returns @racket[#f] if the cryptography library is cached as an
exception, or if @racket[objname] was not found in the library.

The value returned from this function is cached in Racket-managed
memory.
}

@defproc[(crypto-dump-error-queue!) (or/c #f list?)]{
Empties the error queue in the C runtime, then returns the error codes
as a Racket list.

If the cryptography library is currently cached as an exception, this
function will always return @racket[#f].
}

@defproc[(crypto-translate-error! [code exact-integer?]) (or/c #f string?)]{
Returns a human-readable string of the given error code from the
underlying C library, or @racket[#f] if the cryptography library is
currently cached as an exception.

This function operates independently from
@racketmodname[denxi/l10n]. If the output string does not respect the
user's locale, then supply translations to the correct
@racketmodname[denxi/l10n] extention and use that extension instead.
}

@defproc[(crypto-raise!) any]{
Raises @racket[($crypto:error (crypto-dump-error-queue!))].

Note that the side-effect implies movement of the entire error queue
from the C runtime into the raised Racket value. Leverage this when
you have reason to expect that the queue contains @italic{only} the
errors that pertain to a failed operation.
}

@defstruct*[$crypto ()]{
A @tech{message} about a cryptographic operation.
}

@defstruct*[($crypto:error $crypto)
            ([queue (listof exact-integer?)])]{
A low-level cryptographic option failed. @racket[queue] holds all
error codes on the bundled @tt{libcrypto} error queue at the time the
instance was constructed. In the unlikely event of @racket[(null?
queue)], a context trace may be necessary to interpret the root cause.
}
