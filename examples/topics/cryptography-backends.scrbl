#lang scribble/manual

@title[#:tag "cryptography-backends"]{Cryptography Backends}

@require[@for-label[racket]
                    "../shared.rkt"]

Required reading: @secref{integrity}, @secref{signature}


@margin-note{We use Racket's built-in SHA-1 implementation for
compatibility reasons, but this isn't a good idea for production
because Xiden ships with an OpenSSL-backed C library. We avoid it here
for compatibility reasons, since that library may not be available for
your processor's architecture.}

This section shows how to define your own backend for cryptographic
operations. This will help if Xiden's bundled cryptography library
does not load on your system, or if you trust entirely different
tools.

This requires a @tech{launcher} and additional programming knowledge,
because you cannot specify a cryptography backend using the default
launcher or environment variables.


@section{Algorithm Selection}

Like OpenSSL, Xiden considers algorithm names and implementations as
distinct entities.

For example, if you decide to use SHA-384, then that does not commit
you to a particular implementation @italic{of} SHA-384. A system might
actually lack an implementation altogether.

For this reason, Xiden sets precedence rules that prefers
implementations in its bundled library, backed by OpenSSL.  This
library contains a fixed set of implementations.

In the event that library cannot load, Xiden may, when given user
consent, fall back to pure-Racket implementations. This is not
recommended because Racket is not viable for cryptographic operations
in production, but it allows Xiden to remain functional.


@section{Example: Mixed-Mode OpenSSL and GPG}

This module uses the host OpenSSL binary for digest creation and the
host GPG binary for signature creation and verification.

Notice that this impacts the key formats supported by Xiden, because
Xiden inherits capabilities of its backend.

@racketmod[
xiden/launcher

(define gpg
  (or (find-executable-path "gpg")
      (raise-user-error "gpg must be installed and be visible in your PATH")))

(define openssl
  (or (find-executable-path "openssl")
      (raise-user-error "openssl must be installed and be visible in your PATH")))

(define gpg-exec (curry system* gpg))
(define openssl-exec (curry system* openssl))

]
