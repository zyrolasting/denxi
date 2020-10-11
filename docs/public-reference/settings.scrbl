#lang scribble/manual

@(require (for-label racket/base racket/contract xiden/rc xiden/setting)
          (for-syntax "../shared.rkt"
                      racket
                      syntax/stx
                      xiden/rc
                      xiden/setting
                      xiden/cli-flag)
          xiden/rc
          xiden/setting
          xiden/cli-flag
          "../shared.rkt")

@title{Configuration}

@project-name dynamically binds configurable values using
@tech{settings} when launched. A @deftech{runtime configuration} is a
@tech/reference{parameterization} in which every @tech{setting}
defined by @racketmodname[xiden/rc] is bound to a value that cannot be
overridden except by another @tech/reference{parameterization}.


@section{Settings}

@defmodule[xiden/setting]

A @deftech{setting} is an instance of the @racket[setting]
@tech/reference{structure}.  Settings are used as a canonical source
of dynamically bound values, along with validation information and
contextual help.

@defstruct*[setting ([id symbol?] [valid? predicate/c] [parameter parameter?] [derived-parameter parameter?] [description string?])]{
Defines a @tech{setting}. You likely do not need to create an instance
directly because the constructor does not enforce a meaningful
structural relationship between the fields. Use
@racket[define-setting] instead.

@racket[setting] implements @racket[prop:procedure]. For an instance @racket[S]:

@itemlist[
@item{@racket[(S)] is @racket[((setting-derived-parameter S))].}
@item{@racket[(S val proc)] applies the procedure @racket[proc] in a @tech/reference{parameterization} where @racket[(setting-derived-parameter S)] is @racket[val].}
]
}


@defform[(define-setting id contract-expr default-expr description)]{
Binds a new @tech{setting} to @racket[id].

@racket[contract-expr] must evaluate to a @tech/reference{flat contract}.
Any attempt to install a value in the setting that does not pass this
contract will fail.

@racket[default-expr] must evaluate to either a @racket[(-> symbol?
any/c)] procedure, or a non-procedure. The procedure form must accept
@racket[id] (as a symbol) as the sole formal argument and return a
default value.

@racket[description] must evaluate to a string that briefly summarizes
the effect of the setting.

@racketblock[
(define-setting PICKED_NUMBER (integer-in 0 100) 0 "a user's guess for a freshman year project")
]
}

@defproc[(call-with-applied-settings [settings (if/c hash?
                                                     (hash/c setting? any/c)
                                                     (listof (cons/c setting? any/c)))]
                                     [thunk (-> any)])
                                     any]{
Applies @racket[thunk] in a @tech/reference{parameterization} where
each @tech{setting} in @racket[settings] is bound to a new value.

@racketblock[
(define-setting USERNAME string? "" "username")
(define-setting PASSWORD string? "" "password")
(call-with-applied-settings (hasheq USERNAME "insecure" PASSWORD "hunter2") PASSWORD)
(call-with-applied-settings (list (cons USERNAME "insecure") (cons PASSWORD "hunter2")) PASSWORD)
]

}



@section{Runtime Configuration}

@defmodule[xiden/rc]

@racketmodname[xiden/rc] provides several @tech{settings} that change
how @project-name behaves. This section documents each setting with
its command-line flags, contract, and default value.

@subsection{Changing a Runtime Configuration Value}

Here are the ways one can change a setting. Each method overrides
the method before it.

@itemlist[

@item{Do nothing. Every setting has a hard-coded default.}
@item{Set an environment variable, e.g. @litchar{export XIDEN_VERBOSE="#t"}.}
@item{Open @litchar{etc/xiden.rkt} in a @tech{workspace} and add @racket[(define XIDEN_VERBOSE #t)].}
@item{When applicable, use @litchar{--XIDEN_VERBOSE '#t'} in a command line (or an alternative flag).}
@item{In a program, use @racket[(parameterize ([(setting-derived-parameter XIDEN_VERBOSE) #t]) ...)].}

]


@subsection{Setting Reference}

These are the defined settings for @|project-name|, along with their default
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

@defsetting*[XIDEN_SANDBOX_MEMORY_LIMIT_MB]{
Defines the memory quota for sandboxed transactions, in mebibytes.

If this is too low, then it is possible for installations to fail
due to a forced custodian shutdown.
}


@defsetting*[XIDEN_SANDBOX_EVAL_MEMORY_LIMIT_MB]{
Like @racket[XIDEN_SANDBOX_MEMORY_LIMIT_MB], but sets a memory quota for every
expression under evaluation.
}

@defsetting*[XIDEN_SANDBOX_EVAL_TIME_LIMIT_SECONDS]{
Like @racket[XIDEN_SANDBOX_EVAL_MEMORY_LIMIT_MB], but sets a time quota for every
expression under evaluation, in seconds.
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

@defsetting*[XIDEN_TRUST_ANY_PUBLIC_KEY]{
@bold{Dangerous}. When true, trust any public key used to verify a signature.
}

@defsetting[XIDEN_TRUSTED_PUBLIC_KEYS (listof well-formed-integrity-info/c)]{
A list of integrity information used to verify public keys. If a public key
fetched for an input passes the integrity check given an item in
@racket[XIDEN_TRUSTED_PUBLIC_KEYS], then the public key is considered
trustworthy.
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

@defsetting*[XIDEN_PRIVATE_KEY_PATH]{
When set, use the private key at the given path for signing inputs.

Currently not used.
}

@defsetting*[XIDEN_CATALOGS]{
A list of strings representing URL templates.

This setting affects the output of @racket[from-catalogs].
}

@defsetting*[XIDEN_DOWNLOAD_MAX_REDIRECTS]{
The maximum number of HTTP redirects to follow when resolving a GET request.
}

@defsetting*[XIDEN_ALLOW_UNDECLARED_RACKET_VERSIONS]{
When true, continue installing when a @tech{package definition}
does not declare supported versions of Racket.
}

@defsetting*[XIDEN_ALLOW_UNSUPPORTED_RACKET]{
When true, continue installing when a @tech{package definition}
declares that it does not support the running Racket version.
}
