#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/string
                    xiden/integrity
                    xiden/logged
                    xiden/security]
         xiden/security
         @for-syntax[xiden/security]
         "../../shared.rkt"]

@title{Security}

@defmodule[xiden/security]

A Xiden process implicitly trusts its system-level dependencies and
operates under the permissions granted to it by the operating
system. Xiden offers no extensions or modifications to the security
model of the operating system.

The attack surface includes the permissions set on any Racket process
that can use Xiden's bindings, and the @tech/xiden-reference["runtime
configuration"], which ultimately controls arguments to
@racket[restrict] in production use.

@defproc[(restrict [#:memory-limit memory-limit (>=/c 0)]
                   [#:time-limit time-limit (>=/c 0)]
                   [#:trusted-executables trusted-executables (listof well-formed-integrity-info/c)]
                   [#:allowed-envvars allowed-envvars (listof (or/c bytes-environment-variable-name? string?))]
                   [#:implicitly-trusted-host-executables implicitly-trusted-host-executables (listof string?)]
                   [#:trust-any-executable? trust-any-executable? any/c]
                   [#:trust-unverified-host? trust-unverified-host? any/c]
                   [#:workspace workspace path-string?]
                   [#:gc-period gc-period (>=/c 0)]
                   [#:name name (or/c string? symbol?) (or (object-name proc) "")]
                   [halt (-> exit-code/c messy-log/c any)]
                   [proc (-> (-> exit-code/c messy-log/c any) any/c)])
                   logged?]{
Reduces runtime privileges.

Applies @racket[proc] under a new @tech/reference{parameterization},
then sends control to @racket[halt] depending on runtime behavior.

The parameterization includes

@itemlist[

@item{a new @tech/reference{security guard} that prohibits listening
for connections, and any filesystem activity irrelevant to updating a
@racket[workspace]. Only the executables whose digests match the
integrity information in @racket[trusted-executables] may be used to
create subprocesses, unless @racket[trust-any-executable?] is true, or
if the executable's path matches @racket[(find-executable-path E)] for
some @racket[E] in @racket[implicitly-trusted-host-executables].

Any violation caught by the security guard will halt evaluation
of @racket[proc] and create a @racket[$restrict:operation] @tech{message}
on the program log.}

@item{a new @tech/reference{custodian} that, if per-custodian memory
accounting is available, will shut down if it consumes more than
@racket[memory-limit] mebibytes.}

@item{a limited subset of environment variables containing only
@racket[allowed-envvars].}

@item{A value for @racket[current-https-protocol] that depends on @racket[trust-unverified-host?].}

]

@racket[proc] runs in a new thread. If that thread does not
terminate on its own within @racket[time-limit] seconds, then it is
forcibly killed and the program log will include a
@racket[$restrict:budget] message. While the thread is active, garbage
is collected every @racket[gc-period] seconds.

If @racket[proc] returns a value without incident, then the logged
procedure will use that value. Otherwise, the logged procedure will
use @racket[FAILURE] and include the relevant @racket[$restrict]
message with the given @racket[name].
}

@defstruct*[($restrict $message) ([name (or/c string? symbol?)]) #:prefab]{
A @tech{message} used to reports violations of safety limits, where
@racket[name] is equal to the value passed as @racket[name] to
@racket[restrict].
}

@defstruct*[($restrict:budget $restrict)
            ([kind (or/c 'space 'time)]
             [amount (>=/c 0)])
            #:prefab]{
Reports a resource limit violation.

If @racket[kind] is @racket['space], then @racket[amount] is bound to
a value passed as @racket[memory-limit] to @racket[restrict].

If @racket[kind] is @racket['time], then @racket[amount] is bound to
a value passed as @racket[time-limit] to @racket[restrict].
}

@defstruct*[($restrict:operation $restrict)
            ([reporting-guard (or/c 'file 'network 'link)]
             [summary symbol?]
             [args list?])
            #:prefab]{
Reports a security violation.

@racket[reporting-guard] corresponds to a callback used with the
@tech/reference{security guard} that blocked an operation.
@racket[args] is equal to the arguments for that callback at the time
the operation was blocked.

@racket[summary] is a symbol that describes the security decision.
It can be one of the following:

@itemlist[
@item{@racket['blocked-execute]: A request to execute a file was blocked.}
@item{@racket['blocked-write]: A request to write to disk was blocked.}
@item{@racket['blocked-delete]: A request to delete a file was blocked.}
@item{@racket['blocked-listen]: A request to listen for network connections was blocked.}
@item{@racket['blocked-link]: A request to create a symbolic link was blocked.}
]
}

@defsetting*[XIDEN_MEMORY_LIMIT_MB]{
Defines a memory limit for a custodian managing process resources, in
mebibytes.  If this is too low, then it is possible for Xiden to halt
due to a forced custodian shutdown.

Does not count memory charged when parsing the command line and
setting up a @tech{runtime configuration}.

Has no effect if the running Racket installation does not support
per-custodian memory accounting.
}

@defsetting*[XIDEN_TIME_LIMIT_S]{
Sets a time limit for a Xiden process, in seconds. Does not count time
spent parsing the command line and setting up a @tech{runtime
configuration}.
}

@defsetting*[XIDEN_TRUST_CERTIFICATES]{
A list of paths to server certificates that Xiden will trust in
addition to those available in the operating system. This option is
safer than @racket[XIDEN_TRUST_UNVERIFIED_HOST] so long as the
certificates are verified by a trusted party.
}

@defsetting*[XIDEN_TRUST_UNVERIFIED_HOST]{
@bold{Dangerous}. When true, trust any server that was not authenticated using available certificates.
}

@defsetting*[XIDEN_TRUST_ANY_EXECUTABLE]{
@bold{Dangerous}. When true, allow the Racket runtime to start a subprocess with any executable.
}

@defsetting[XIDEN_TRUST_EXECUTABLES (listof well-formed-integrity-info/c)]{
Like @racket[XIDEN_TRUST_PUBLIC_KEYS], but used to verify
executables a @tech{package} tries to use when creating a subprocess.

Beware: Any executable listed here inherits the OS-level permissions
of the process, and is not subject to the restrictions of a
Xiden @tech{runtime configuration}.  If you include a
Xiden launcher or a sufficiently flexible Racket launcher, a
@tech{package} can start a new Xiden process with a full-trust
configuration.
}

@defsetting*[XIDEN_TRUST_HOST_EXECUTABLES]{
Like @racket[XIDEN_TRUST_EXECUTABLES], except this setting is a list
of names. Xiden will allow execution of a file if its normalized path
equals the value of @racket[find-executable-path] for an element of
that list. You may need to add multiple entries to account for
extension differences across platforms.

This can be helpful in the event a package depends on access to an
executable on the host system and there is no way to control the
content of that executable.

The @racket[find-executable-path] restriction is meant to prevent
packages from creating and then immediately running their own
executables just because they have a name in this list. Even so, this
can be a dangerous setting, and should only be used if you trust both
the package definition and the executables on your system. It's also
why @tt{PATH} should not include a build directory.

Regardless of the setting's actual value, Xiden implicitly considers
@racket{openssl} an element of its list. The user is therefore
responsible for the integrity of their OpenSSL instance.
}

@defsetting*[XIDEN_ALLOW_ENV]{
Names of environment variables visible to @tech{packages}, and
Xiden subprocesses.

@racket{PATH} is included regardless of the value of this setting.
}
