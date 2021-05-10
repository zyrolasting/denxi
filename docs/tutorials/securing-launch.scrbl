#lang scribble/manual

@require["../shared.rkt"
         @for-label[racket/base
                    xiden/cli
                    xiden/integrity
                    xiden/security
                    xiden/source]]

@title[#:tag "securing-launch"]{Securing Xiden}

This tutorial is aimed at engineers who are either trained in
information security, or are at least conscious of how trust impacts
safety in programs. We'll cover how to interact with Xiden's
zero-trust configuration, and then how to safely encode and distribute
a launcher that conveniently includes exactly what one @italic{can}
trust.

Xiden's draws configuration from environment variables, command-line
options, and adjustments made by a custom @tech/xiden-guide{launcher}.

Like any other process, a Xiden process' true restrictions come from
the operating system. Running Xiden as root or an administrator
carries risk, which is why it was designed for unprivileged
use. @bold{Running Xiden with elevated privileges and safety checks
disabled is equivalent to allowing arbitrary code execution.}


@section{Check Implicit Trust First!}

The edition of Xiden distributed through the Racket package catalog
implicitly trusts the Racket installation, OpenSSL instance, and
SQLite instance installed on the host system. It also implicitly
trusts its own @tech/xiden-reference{state}. If any of these are
compromised, then Xiden is compromised.

The claims made in this tutorial assume that you can trust Xiden's
host-level dependencies and the low-level libraries powering them.  If
that assumption does not hold, then you'll need to fix your system
before continuing the tutorial.

When you are satisfied with your system, you still have some thinking
to do. Trust is complicated because granting explicit trust for one
situation means granting implicit trust in other situations.

For example, one of the most dangerous settings in Xiden is
@racket[XIDEN_TRUST_BAD_DIGEST]. When enabled, it allows Xiden to
trust data even if it fails an integrity check.

In plain language, trusting data despite integrity failure is the same
as saying that you trust @italic{arbitrary} data. And if you trust
arbitrary data, then there's no point in checking a signature for the
data. This means that a Xiden process that trusts bad digests will
download information, and you won't really know what that information
is until you look at it yourself.

But even when Xiden trusts bad digests, it will never do so for
executables unless another setting overrides it. This is because the
only thing worse that downloading some mysterious blob is executing
it.

For example, if @racket[XIDEN_TRUST_BAD_DIGEST] is true, Xiden will
still not execute any subprocess outside of the host's OpenSSL
instance unless it has an entry in @racket[XIDEN_TRUST_EXECUTABLES].
A particularly trusting programmer can shut off that side of Xiden's
protections by enabling @racket[XIDEN_TRUST_ANY_EXECUTABLE].

You can probably start to see the pattern here, so, I'll skip to the
end: @bold{This configuration disables all safety features in Xiden.}

@racketblock[
(XIDEN_TRUST_ANY_EXECUTABLE #t)
(XIDEN_TRUST_BAD_DIGEST #t)
(XIDEN_TRUST_UNVERIFIED_HOST #t)
(XIDEN_MEMORY_LIMIT_MB +inf.0)
(XIDEN_TIME_LIMIT_S +inf.0)
(XIDEN_FETCH_TIMEOUT_MS +inf.0)
(XIDEN_FETCH_TOTAL_SIZE_MB +inf.0)
]

In this configuration, HTTP traffic is allowed, servers using HTTPS
are not authenticated, any input is trusted, and any subprocess may
execute. Furthermore, it may use any amount of RAM or CPU time, and
may download without limits.

Each setting can be useful on its own in early prototyping contexts,
or when a safety limit (somehow) prevents legitimate business with a
trusted party.  In any other situation, this configuration should be
considered as reckless and offensive as @litchar|{curl --insecure |
sudo bash}|.


@section{How to Trust No One}

The built-in @litchar{xiden} launcher starts with hard-coded defaults
for a zero-trust configuration, but allows trust through environment
variables and command-line options.

Zero-trust offers some key benefits:

@itemlist[
@item{It takes effort to add risk.}

@item{You can run Xiden with untrusted code and inspect errors
as a way to learn what you are expected to trust.}
]

To fully enjoy these benefits, create a user in your operating system
that has execute permissions for @litchar{xiden}, but write
permissions only for its home directory. Possibly with resource
quotas.

A true zero-trust state will not have any environment variable
beginning with @litchar{XIDEN_} defined. In a Unix-like system, you
can set that up using @litchar{env -i} or
@litchar{unset}. Alternatively, you can run @litchar{xiden} using
@litchar{su} on the restricted user.

If done correctly, malicious code is subject entirely to process- and
user-level restrictions in the event the Xiden process is compromised.
From here, only new envvars and command-line options will disable
safety checks. This is why it takes manual effort to allow malicious
code to run.

If you are interacting with untrusted code, leverage these zero-trust
conditions to iteratively build an exact trust profile as a shell
script or as a custom @tech/xiden-reference{launcher}.


@section{How to Learn to Trust Again}

While helpful for auditing, a zero-trust launch is not useful for the
working programmer. Once you are aware of what you can safely trust,
you can decide how to leverage the configuration others need to work.

You can do that by either distributing a shell script that configures
the @litchar{xiden} launcher, but it would probably make more sense to
distribute a custom launcher. For one thing, custom launchers are
written in Racket, so they will work on Windows, Mac, and GNU/Linux
distributions outright. Custom launchers also expose other
configuration options that @litchar{xiden} simply does not allow, such
as the ability to automatically resolve @tech{package conflicts}.

But how can you distribute a custom launcher to your team and detect
any tampering? Use Xiden! Write a @tech/xiden-guide["package
definition"] that downloads your custom launcher, with integrity
information built from a digest made with a trusted CHF, with that
digest signed by your private key. Then, require your teammates to use
@litchar{xiden} to install your package definition. They'll only get
the launcher when their Xiden installation verifies your signature.

As an aside: you can probably start to appreciate how Xiden allows
freely-associating people to set their own conventions for
distribution. Since launchers allow you to define your own CLI, you
can add custom commands that make Xiden easier for others to use.

So what if a teammate just disables security checks and ends up with a
compromised launcher? Unfortunately, there is nothing stopping a
teammate from @italic{intentionally} using a permissive Xiden launcher
that allows dangerous code to run on a private network.  But if they
can run dangerous code on your network at will, then they don't need
Xiden to do so. Just know that if they do use Xiden, it takes
@italic{more} effort to have it trust a compromised launcher because
they have to intentionally shut off safety checks. I'd bet you'd
notice if they did that.


@section{Securing Xiden: Recap}

We discussed a general process that allows an InfoSec specialist to
build a tailored trust profile, and encode it in a launcher for safe
distribution using Xiden's zero-trust launcher. This allows not only
higher confidence in custom software distribution, but a way to ease
the user experience for other developers without sacrificing security
considerations.
