#lang denxi/document

@title{Security}

@defmodule[denxi/security]

A Denxi process implicitly trusts its system-level dependencies and
operates under the permissions granted to it by the operating
system. Denxi offers no extensions or modifications to the security
model of the operating system.

The attack surface includes the permissions set on any Racket process
that can use Denxi's bindings, and the @tech/denxi-reference["runtime
configuration"], which ultimately controls arguments to
@racket[restrict] in production use.

@defproc[(restrict [#:memory-limit memory-limit (>=/c 0)]
                   [#:time-limit time-limit (>=/c 0)]
                   [#:trusted-executables trusted-executables (listof well-formed-integrity?)]
                   [#:allowed-envvars allowed-envvars (listof (or/c bytes-environment-variable-name? string?))]
                   [#:implicitly-trusted-host-executables implicitly-trusted-host-executables (listof string?)]
                   [#:trust-any-executable? trust-any-executable? any/c]
                   [#:trust-unverified-host? trust-unverified-host? any/c]
                   [#:gc-period gc-period (>=/c 0)]
                   [#:name name (or/c string? symbol?) (or (object-name proc) "")]
                   #:writeable-directories writeable-directories
		   [halt (-> exit-code/c subprogram-log/c any)]
                   [proc bound-program/c])
                   subprogram?]{
Applies @racket[proc] under a new @tech/reference{parameterization},
with restricted runtime privileges. Eventually sends control to
@racket[halt] depending on runtime behavior.

The parameterization includes

@itemlist[

@item{a new @tech/reference{security guard} that prohibits listening
for connections, and limits filesystem writes to
@racket[writeable-directories]. Only the executables whose digests
match the integrity information in @racket[trusted-executables] may be
used to create subprocesses, unless @racket[trust-any-executable?] is
true, or if the executable's path matches
@racket[(find-executable-path E)] for some @racket[E] in
@racket[implicitly-trusted-host-executables].

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

If @racket[proc] returns a value without incident, then the subprogram
procedure will use that value. Otherwise, the subprogram will
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
