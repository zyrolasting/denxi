#lang scribble/manual

@title{OpenSSL}

@require[@for-label[racket/base
                    racket/contract
                    racket/pretty
                    xiden/openssl]
         racket/pretty
         @for-syntax[xiden/openssl]
         xiden/openssl
         "../../shared.rkt"]

@defmodule[xiden/openssl]

OpenSSL is an implicitly-trusted dependency that Xiden invokes as a
subprocess. @racketmodname[xiden/openssl] also defines data and
contracts for @deftech{cryptographic hash functions} (or
@deftech{CHF}s).


@defthing[DEFAULT_CHF chf/c #:value 'sha3-384]{
A fallback cryptographic hash function.

This value is subject to change for security reasons, and is not
guarenteed to be available in the installed OpenSSL implementation.
}

@defthing[chf/c flat-contract? #:value (apply or/c cryptographic-hash-functions)]{
A contract that accepts one of the symbols in @racket[cryptographic-hash-functions].
}

@defthing[md-bytes-source/c flat-contract? #:value (or/c path-string? bytes? input-port?)]{
This contract matches a value @racket[V] suitable for use in @racket[make-digest].

Given @racket[(path-string? V)], the bytes are drawn from the file located at @racket[V].
Given @racket[(bytes? V)] or @racket[(input-port? V)], the bytes are drawn directly from @racket[V].
}

@defthing[cryptographic-hash-functions (listof symbol?)]{
A list of symbols that represent supported message digest algorithms,
a.k.a. cryptographic hash functions.

Bound to @typeset-code[(pretty-format #:mode 'print cryptographic-hash-functions)]
}

@defsetting*[XIDEN_TRUST_CHFS]{
A list of trusted cryptographic hash function implementations in
OpenSSL.
}

@defthing[openssl complete-path?]{
Equal to @racket[(find-executable-path "openssl")].
}

@defproc[(make-digest [variant md-bytes-source/c] [algorithm chf/c DEFAULT_CHF]) bytes?]{
Returns the raw byte content of @racket[algorithm] applied to bytes from @racket[variant].
}

@defstruct*[($openssl-error $message) ([args list?]
                                       [timeout (or/c #f (>=/c 0))]
                                       [exit-code exact-nonnegative-integer?]
                                       [output bytes?]
                                       [reason (or/c #f bytes?)])]{
Represents a failed OpenSSL subprocess using the given arguments.

If @racket[timeout] is not @racket[#f], then the subprocess timed out
after Xiden waited @racket[timeout] seconds.

@racket[exit-code] represents the exit code returned from the process.

@racket[output] is a dump of the process' standard output at the time
of failure. @racket[reason] holds a dump of standard error bytes, or
@racket[#f] if the process timed out.
}
