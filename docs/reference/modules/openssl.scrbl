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


@defthing[DEFAULT_CHF chf/c]{
A fallback cryptographic hash function to use when one is not
specified.

While you can use this value as a constant, it isn't a constant.  The
value is subject to change for security reasons, and is based on CHFs
available in the host's OpenSSL instance.
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
A list of symbols that represent cryptographic hash functions,
available in the underlying OpenSSL instance.

Bound to @typeset-code[(pretty-format #:mode 'print cryptographic-hash-functions)]
}

@defsetting*[XIDEN_TRUST_CHFS]{
A list of trusted cryptographic hash functions. If they are available
in the underlying OpenSSL instance, they may be used.
}

@defthing[openssl complete-path?]{
Equal to @racket[(find-executable-path "openssl")].
}

@defproc[(make-digest [variant md-bytes-source/c] [chf symbol? DEFAULT_CHF]) bytes?]{
Returns the raw byte content of @racket[algorithm] applied to bytes from @racket[variant].

Raises @racket[$openssl:unavailable-chf] if the underlying OpenSSL
instance does not support @racket[chf].
}

@defproc[(get-available-chfs) (listof symbol?)]{
Returns a list of cryptographic hash functions supported by the host's
OpenSSL instance.
}

@defstruct*[($openssl $message) ()]{
A @tech{message} pertaining to OpenSSL use.
}

@defstruct*[($openssl:unavailable-chf $openssl) ([requested symbol?])]{
The @racket[requested] CHF is not available in the OpenSSL build on
the host system.
}

@defstruct*[($openssl:error $openssl) ([args list?]
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
