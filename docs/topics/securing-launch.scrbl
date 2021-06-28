#lang scribble/manual

@require["../shared.rkt"
         @for-label[racket/base
                    xiden/cli
                    xiden/integrity
                    xiden/security
                    xiden/source]]

@title[#:tag "securing-launch"]{Securing Xiden}

This tutorial is aimed at engineers who are at least conscious of how
trust impacts safety in programs. We'll cover how to interact with
Xiden's zero-trust configuration, and then how to safely encode and
distribute a launcher that conveniently includes exactly what one
@italic{can} trust.

Xiden's draws configuration from environment variables, command-line
options, and adjustments made by a custom @tech/xiden-guide{launcher}.

Like any other process, a Xiden process' true restrictions come from
the operating system. Running Xiden as root or an administrator
carries risk, which is why it was designed for unprivileged
use. @bold{Running Xiden with elevated privileges and safety checks
disabled is equivalent to allowing arbitrary code execution.}


@section{Check Implicit Trust}

The edition of Xiden distributed through the Racket package catalog
implicitly trusts its own direct dependencies and
@tech/xiden-reference{state}. If any of these are compromised, then
Xiden is compromised.

The claims made here assume that you can trust Xiden's host-level
dependencies and the low-level libraries powering them.  If that
assumption does not hold, then you'll need to fix your system before
continuing.


@section{Check Implications of Trust}

Trust is complicated because granting explicit trust for one situation
means granting implicit trust in other situations. For example, one of
the most dangerous settings in Xiden is
@racket[XIDEN_TRUST_BAD_DIGEST]. When enabled, it allows Xiden to
trust data even if it fails an integrity check.

In plain language, trusting data despite integrity failure is the same
as saying that you trust @italic{arbitrary} data. And if you trust
arbitrary data, then there's no point in checking a signature for the
data. This means that a Xiden process that trusts bad digests will
download information, and you won't really know what that information
is until you look at it yourself.

But even when Xiden trusts bad digests, it will never execute
arbitrary data in a subprocess unless another setting disables that
check.

For example, if @racket[XIDEN_TRUST_BAD_DIGEST] is true, Xiden will
still not execute any subprocess unless it has a matching entry in
@racket[XIDEN_TRUST_EXECUTABLES].  A particularly trusting programmer
can shut off that side of Xiden's protections by enabling
@racket[XIDEN_TRUST_ANY_EXECUTABLE].

You can probably start to see the pattern here, so, I'll skip to the
end: @bold{This launcher disables all safety features in Xiden.}

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

This launcher allows HTTP traffic, servers that fail HTTPS
authentication, any input, and any subprocess spawn. Furthermore, the
launcher may use any amount of memory, may take as much time as it
wants to finish, and may download without limits. The only
restrictions on this process are limited to the user's restrictions on
the operating system.

Each setting can be useful on its own in early prototyping contexts,
or when a safety limit somehow prevents legitimate business with a
trusted party.  In any other situation, this launcher is as reckless
as @litchar|{curl --insecure | sh}|.


@section{Trust Incrementally}

The built-in @litchar{xiden} launcher starts with hard-coded defaults
for a zero-trust configuration, but allows trust through environment
variables and command-line options.

A true zero-trust state will not have any environment variable
beginning with @litchar{XIDEN_} defined. In a Unix-like system, you
can set that up using @litchar{env -i} or
@litchar{unset}. Alternatively, you can run @litchar{xiden} using
@litchar{su} on a restricted user.

If you are interacting with untrusted code, leverage these zero-trust
conditions to iteratively build an exact trust profile as a shell
script or as a custom @tech/xiden-reference{launcher}.

A zero-trust launch is useful for increasing confidence, but not for
some day-to-day work. This is why the documentation makes such a big
deal over distributing custom launchers using the built-in launcher.

What if a teammate just disables security checks and ends up with a
compromised launcher? Unfortunately, there is nothing stopping a
teammate from @italic{intentionally} using a permissive Xiden launcher
that allows dangerous code to run on a private network.  But if they
can run dangerous code on your network at will, then they don't need
Xiden to do so. Just know that if they do use Xiden, it takes
@italic{more} effort to have it trust a compromised launcher because
they have to intentionally shut off safety checks. I'd bet you'd
notice if they did that.

We discussed a general process for building a tailored trust profile,
and encode it in a launcher for safe distribution using Xiden's
zero-trust launcher. This allows not only higher confidence in custom
software distribution, but a way to ease the user experience for other
developers without sacrificing security considerations.
