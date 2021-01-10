#lang scribble/manual

@(require (for-label racket
                     xiden/integrity
                     xiden/rc
                     xiden/setting
                     xiden/url)
          (for-syntax "../../shared.rkt"
                      racket
                      syntax/stx
                      xiden/rc
                      xiden/setting
                      xiden/cli-flag)
          xiden/rc
          xiden/setting
          xiden/cli-flag
          "../../shared.rkt")

@title{Runtime Configuration}

@defmodule[xiden/rc]

@racketmodname[xiden/rc] provides several @tech{settings} that change
how Xiden behaves. This section documents each setting with
its command-line flags, contract, and default value.

@section{Changing a Runtime Configuration Value}

Here are the ways one can change a setting. Each method overrides
the method before it.

@itemlist[
@item{Do nothing. Every setting has a hard-coded default.}
@item{Set an environment variable, e.g. @litchar{export XIDEN_VERBOSE="#t"}.}
@item{Open an @tech{rcfile} and add @racket[(define XIDEN_VERBOSE #t)].}
@item{When applicable, use @litchar{--XIDEN_VERBOSE '#t'} in a command line (or an alternative flag).}
@item{In a program, use @racket[(XIDEN_VERBOSE #t (lambda () ...))].}
]

@include-section{rcfile.scrbl}

@section{Runtime Configuration API}

@defthing[XIDEN_SETTINGS (hash/c symbol? setting? #:immutable #t)]{
A hash table of all defined @tech{settings}, such that the key for
each setting is @racket[(setting-id S)].
}

@defproc[(call-with-rcfile [thunk (-> any)]) any]{
Calls @racket[thunk] in a @tech/reference{parameterization} where
fallback values for @tech{settings} consider values from the
@tech{target rcfile}.

Each call to @racket[call-with-rcfile] reads the content of the
@tech{target rcfile} into memory.
}

@defproc[(dump-xiden-settings) (hash/c symbol? any/c)]{
Returns a hash table containing the value of every @tech{setting} in
@racket[XIDEN_SETTINGS] in the current @tech/reference{parameterization}.
}

@section{Setting Reference}

These are the defined settings for Xiden, along with their default
values and command-line flags.

@(define-for-syntax (infer-contract-expr stx s)
   (define proc (setting-valid? s))
   (define formatted (~v proc))
   (reformat-syntax stx
     (if (string-prefix? formatted "#<")
         (object-name proc)
         (read (open-input-string formatted)))))

@(define-syntax (defsetting stx)
  (syntax-case stx ()
    [(_ s cnt pre-content ...)
      #`(let ([cf (find-cli-flag s)])
          (defthing
            #:kind "setting"
            s cnt
            #:value #,(datum->syntax stx (eval #'(s)) stx)
            (para "CLI Flags: "
                  (if cf
                      (litchar (format-cli-flags cf))
                      "N/A"))
            pre-content ...))]))

@(define-syntax (defsetting* stx)
  (syntax-case stx ()
    [(_ s pre-content ...)
      #`(defsetting s #,(infer-contract-expr stx (eval #'s)) pre-content ...)]))

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

@defsetting*[XIDEN_INSTALL_SOURCES]{
Defines installations in a transaction.

Each list in @racket[XIDEN_INSTALL_SOURCES] consists of three strings:

@itemlist[#:style 'ordered
@item{The path to a symbolic link to create with respect to @racket[(current-directory)].}
@item{The name of a desired output from a @tech{package definition}.}
@item{A URL, file path, or plugin-specific string used to find the @tech{package definition}.}
]

}

@defsetting*[XIDEN_INSTALL_ABBREVIATED_SOURCES]{
Like @racket[XIDEN_INSTALL_SOURCES], except each item in the list only needs to
be a URL, file path, or plugin-specific string used to find the @tech{package
definition}. The symbolic link name is assumed to be the string bound to
@racketfont{package} in the definition, and the output is assumed to be
@racket{default}.
}

@defsetting*[XIDEN_INSTALL_DEFAULT_SOURCES]{
Like @racket[XIDEN_INSTALL_SOURCES], except each list only needs two strings:


@itemlist[#:style 'ordered
@item{The path of a symbolic link to create with respect to @racket[(current-directory)].}
@item{A URL, file path, or plugin-specific string used to find the @tech{package definition}.}
]

The output is assumed to be @racket{default}.
}

@defsetting*[XIDEN_PLUGIN_MODULE]{
When not @racket[#f], the given module path will be used in @racket[dynamic-require]
to load extensions.
}

@defsetting*[XIDEN_TRUST_UNSIGNED]{
@bold{Dangerous}. When true, trust any input that lacks a signature.
}

@defsetting*[XIDEN_TRUST_BAD_SIGNATURE]{
@bold{Dangerous}. When true, trust any input that has a signature that does not match the input's integrity information.
}

@defsetting*[XIDEN_TRUST_UNVERIFIED_HOST]{
@bold{Dangerous}. When true, trust any server that was not authenticated using available certificates.
}

@defsetting*[XIDEN_TRUST_BAD_DIGEST]{
@bold{Dangerous}. When true, trust any input.
}

@defsetting*[XIDEN_TRUST_ANY_EXECUTABLE]{
@bold{Dangerous}. When true, allow the Racket runtime to start a subprocess with any executable.
}

@defsetting*[XIDEN_TRUST_ANY_PUBLIC_KEY]{
@bold{Dangerous}. When true, trust any public key used to verify a signature.
}

@defsetting[XIDEN_TRUSTED_PUBLIC_KEYS (listof well-formed-integrity-info/c)]{
A list of integrity information used to verify public keys. If a
public key fetched for an input passes the integrity check for an
element of @racket[XIDEN_TRUSTED_PUBLIC_KEYS], then the public key is
considered trustworthy.
}

@defsetting[XIDEN_TRUSTED_EXECUTABLES (listof well-formed-integrity-info/c)]{
Like @racket[XIDEN_TRUSTED_PUBLIC_KEYS], but used to verify
executables a @tech{package} tries to use when creating a subprocess.

Beware: Any executable listed here inherits the OS-level permissions
of the process, and is not subject to the restrictions of a
Xiden @tech{runtime configuration}.  If you include a
Xiden launcher or a sufficiently flexible Racket launcher, a
@tech{package} can start a new Xiden process with a full-trust
configuration.

@bold{Regardless of this setting, Xiden implicitly trusts the
binary referenced by @racket{openssl} in your @tt{PATH}.} This will
not be the case in a future release.
}


@require[@for-label[racket/fasl racket/serialize]]
@defsetting*[XIDEN_FASL_OUTPUT]{
When true, each value @racket[v] printed on STDOUT is first transformed using
@racket[(s-exp->fasl (serialize v))].
}

@defsetting*[XIDEN_READER_FRIENDLY_OUTPUT]{
When true, each program output value @racket[v] is printed on STDOUT using
@racket[pretty-write] without being translated to a human-readable message.

Use this to produce @racket[(read)]able logs. If it aids read performance,
combine with @racket[XIDEN_FASL_OUTPUT].
}

@defsetting*[XIDEN_FETCH_TOTAL_SIZE_MB]{
The maximum total size of a single download allowed when fetching an input from
a source, in mebibytes.
}

@defsetting*[XIDEN_FETCH_BUFFER_SIZE_MB]{
The maximum number of bytes to read at a time from a source, in mebibytes.
}

@defsetting*[XIDEN_FETCH_PKGDEF_SIZE_MB]{
Like @racket[XIDEN_FETCH_TOTAL_SIZE_MB], except the quota only applies
to @tech{package definitions} named in a user-defined transaction.
This quote does not apply to @tech{package definitions} listed
as inputs in another @tech{package definition}.
}

@defsetting*[XIDEN_FETCH_TIMEOUT_MS]{
The maximum number of seconds to wait for the next available byte from a
source.
}

@defsetting*[XIDEN_VERBOSE]{
When true, emit more detailed program output.
}

@defsetting*[XIDEN_CATALOGS]{
A list of strings representing URL templates.

This setting affects the output of @racket[from-catalogs].
}

@defsetting*[XIDEN_DOWNLOAD_MAX_REDIRECTS]{
The maximum number of HTTP redirects to follow when resolving a GET request.
}

@defsetting*[XIDEN_ALLOW_UNSUPPORTED_RACKET]{
When true, continue installing when a @tech{package definition}
declares that it does not support the running Racket version.
}

@defsetting*[XIDEN_ALLOW_ENV]{
Names of environment variables visible to @tech{packages}, and
Xiden subprocesses.

@racket{PATH} is included regardless of the value of this setting.
}

@defsetting*[XIDEN_SUBPROCESS_TIMEOUT_S]{
The maximum number of seconds a subprocess spawned by a @tech{package}
may live.
}

@defsetting*[XIDEN_INPUT_OVERRIDES]{
A list of strings used to define package input overrides.

Each element is in the form @racket[(cons pattern input-expr)]. The
input of name @racketid[input-name] is replaced with the (evaluated)
@racketid[input-expr] for all @tech{package queries} matching
@racket[pattern].

If @racket[pattern] is a string, then it is used as an argument to
@racket[pregexp] before matching. If @racket[pattern] is a symbol,
then it is first coerced to a string and then used as an argument to
@racket[pregexp].
}

@defsetting*[XIDEN_GENERATED_INPUT_NAME]{
The name to use for generated @tech{package input} code.
}

@defsetting*[XIDEN_BYTE_ENCODING]{
The encoding to use when generating byte expressions.
}

@defsetting*[XIDEN_MESSAGE_DIGEST_ALGORITHM]{
The algorithm to use when creating digests on user request.
}

@defsetting*[XIDEN_SIGNER]{
A list containing, in order, a @tech{source} for a public key, a path
to a private key file, and a path to a file containing the password
for the private key. Any element can be @racket[#f] to indicate
absense.

Xiden will attempt to sign input expressions when at least a public
key source and private key is available.
}
