#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base xiden/rc]]

@title[#:tag "config"]{Configuration}

@define[change-val @secref["Changing_a_Runtime_Configuration_Value"
         #:doc '(lib "xiden/docs/reference/xiden-reference.scrbl")]]

You can configure @|binary| using environment variables, the command
line interface, and/or a runtime configuration file. Every setting has
only one name, and a contract for accepted Racket values.

The @change-val section covers how to change the value of a setting.

@section{Runtime Configuration File}

Each @tech{workspace} can have its own runtime configuration file, or rcfile.
When @project-name selects a @tech{workspace} directory, it will load
@litchar{etc/xiden.rkt} and use it as a source of setting values. Here's an
example of what that file might look like:

@racketmod[
xiden

(define XIDEN_SANDBOX_EVAL_MEMORY_LIMIT_MB 200)
(define XIDEN_VERBOSE #f)
(define XIDEN_TRUST_UNVERIFIED_HOST #f)
(define XIDEN_TRUST_UNSIGNED #t)
(define XIDEN_FETCH_PKGDEF_SIZE_MB 0.1)
(define XIDEN_TRUST_BAD_DIGEST #f)
(define XIDEN_ALLOW_UNSUPPORTED_RACKET #f)
(define XIDEN_SANDBOX_EVAL_TIME_LIMIT_SECONDS 300)
(define XIDEN_FASL_OUTPUT #f)
(define XIDEN_FETCH_TIMEOUT_MS 3000)
]


Notice that the rcfile also uses the @racketmodname[xiden] language, which
allows you to use it as a @tech{package definition}. This is only really useful
for self-hosted @project-name installations, or installations that are unique
to the configured @tech{workspace}.

This does not imply that any @tech{package definition} can act as an rcfile. An
rcfile @italic{must} be located in at @litchar{etc/xiden.rkt} with regards to a
@tech{workspace} directory.


@section{Why Allow Verbose Commands?}

A consequence of the rules in @change-val is that you cannot combine short
flags into strings like @litchar{-vUi}. @italic{Every} flag requires an
argument, so the flags may appear as @litchar{-v '#t' -U '#f' -i '#t'}.

Verbose commands are more painful to type, so why do it this way?
Because there are a few benefits to this approach:

@itemlist[

@item{
Passing values to flags keeps the entire command explicit and self-describing.
Let's say @racket[XIDEN_VERBOSE] is @racket[#t] due to an environment
variable or rcfile. Assuming that you do not change the environment variable,
you would not be able to change that back to @racket[#f]
in a command line without expressing @racket[#f].
}

@item{
What you type is what gets used at runtime, so long as it meets a
contract. This makes commands easier to predict for Racket
programmers, because they can visualize exact values at play.
}

@item{
Every setting has a canonical name, meaning that no matter where
you define a value, you can use the same name. Short flags
are merely bound to the same setting as the flags with
canonical names.

@racketblock[
(define XIDEN_VERBOSE #t)
]

@verbatim|{
$ XIDEN_VERBOSE="#t" xiden ...
}|

@verbatim|{
# Equivalent
$ xiden --XIDEN_VERBOSE "#t" ...
$ xiden -v "#t" ...
}|
}

]


@section{Setting the Workspace}

@racket[XIDEN_WORKSPACE] defines the directory to use as the @tech{workspace}.

@racket[XIDEN_WORKSPACE] can only be set using the environment
variable of the same name. This is because all other settings check a
@tech{workspace}-specific configuration file, and that file cannot be
known until @racket[XIDEN_WORKSPACE] is set.

@racket[XIDEN_WORKSPACE] must be a @racket[complete-path?]. If the
path points to an existing entry on the filesystem, then that entry
must be a directory. If the path points to no existing entry, then
@binary will create a directory at that path.

@section[#:tag "trusting-pubkeys"]{Trusting Public Keys and Executables}

You can use @|project-name|'s integrity checking system to specify
public keys and executables that you trust. You specify the integrity
information the same way that you would for an input in a
@tech{package definition}.

This example value for @racket[XIDEN_TRUSTED_PUBLIC_KEYS] includes the
integrity information for my own public key located at
@visible-hyperlink{https://sagegerard.com/public.pem}.

@racketblock[
(define XIDEN_TRUSTED_PUBLIC_KEYS
  (list (integrity 'sha384
                   (hex "d925eca70c5adfa1d7722cf6c1fb667ed3e7967715a4eaecf52e342663f88231de39f96293e719a27ade3d87666ae54"))))
]

Notice that this is not a fingerprint, it is @italic{integrity information for
an entire (trusted) public key file}. This avoids collision attacks due to
short fingerprint lengths, but not collision attacks based on the digest
algorithm.  If a digest algorithm is subject to a collision attack, you can
upgrade the algorithm and expected digest.

This example value for @racket[XIDEN_TRUSTED_EXECUTABLES] similarly
verifies binaries that @project-name may use for a subprocess. This
example shows integrity information for a Python 2.7 binary.

@racketblock[
(define XIDEN_TRUSTED_EXECUTABLES
  (list (integrity 'sha1 (hex "3b1c5bcf6d6c0f584a07aed26ad18299b5a8311d"))))
]

Note that the integrity information applies only to the named
executable.  This system will not detect something like a compromised
dynamically-linked library.
