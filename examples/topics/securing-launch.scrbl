#lang scribble/manual

@require["../shared.rkt"
         @for-label[racket/base
                    xiden/cli
                    xiden/integrity
                    xiden/security
                    xiden/source]]

@title[#:tag "securing-launch"]{Securing Xiden}

Xiden defines no identity management, so a process running Xiden
depends on OS user permissions. Running Xiden as root or an
administrator is a bad idea.

Xiden implicitly trusts its own direct dependencies and
@tech/xiden-reference{state}. If any of these are compromised, then
Xiden is compromised.

Granting too much trust has ripple effects. For example, one of the
most dangerous settings in Xiden is
@racket[XIDEN_TRUST_BAD_DIGEST]. When enabled, it allows Xiden to
trust data even if it fails an integrity check. This trust implicitly
defeats signature checking for verification.

I'll skip to the end: @bold{This launcher disables all safety features
in Xiden.}

@racketmod[
xiden/launcher

(XIDEN_TRUST_ANY_EXECUTABLE #t)
(XIDEN_TRUST_BAD_DIGEST #t)
(XIDEN_TRUST_UNVERIFIED_HOST #t)
(XIDEN_MEMORY_LIMIT_MB +inf.0)
(XIDEN_TIME_LIMIT_S +inf.0)
(XIDEN_FETCH_TIMEOUT_MS +inf.0)
(XIDEN_FETCH_TOTAL_SIZE_MB +inf.0)

(module+ main (launch-xiden!))
]

If you read the linked settings, you'll see that this launcher is as
reckless as @litchar|{curl --insecure | sh}|.
