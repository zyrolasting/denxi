#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    xiden/message
                    xiden/integrity]
         "../shared.rkt"]

@title{Message Structures}

@defmodule[xiden/message]

A @deftech{message} is an instance of the @racket[$message]
@tech/reference{structure} used to share data in the @project-name
runtime and between Racket processes. @racket[$message] and all of its
subtypes are @tech/reference{prefab} @tech/reference{structures}.
When the term “@tech{message}” is ambiguous, then prefer the term
@deftech{@project-name message} to apply the context of this section.

All @tech{message} types form a heirarchy using colon-separated
identifiers that start with @racket[$]. @racket[$message] itself has
no semantics beyond serving as the root type, and its identifier does
not appear in other structure type identifiers like @racket[exn]'s
does. For example, identifiers pertaining to command line messages
start with @tt{$cli}, not @tt{$message:cli}.

@defstruct*[$message () #:prefab]{
The base type for all @project-name messages.
}

@defform*[((define-message id [field-expr ...])
           (define-message id super-id [field-expr ...]))]{
Like @racket[struct], in that @racket[(define-message foo (a b c))] is
equivalent to @racket[(struct foo $message (a b c) #:prefab)].

The second form allows declaration of a supertype, but that supertype
must be a subtype of @racket[$message].
}

@defform[(define+provide-message id form ...)]{
Like @racket[define-message], with an included @racket[(provide (struct-out id))].
}

@defstruct*[($show-datum $message) ([value any/c]) #:prefab]{
Represents a request to show the user the given Racket value.
}

@defstruct*[($show-string $message) ([message any/c]) #:prefab]{
Represents a request to show the user the given string.
}


@section{Program Results}

@defstruct*[($finished-collecting-garbage $message) ([bytes-recovered exact-nonnegative-integer?]) #:prefab]{
Returned from @|project-name|'s garbage collector. Reports the number
of bytes freed from disk as a result of the operation.
}

@section{High-level Messages}

@defstruct*[($fail $message) ([v any/c]) #:prefab]{
Represents a general failure. When
@racket[XIDEN_READER_FRIENDLY_OUTPUT] is @racket[#f], this message is
presented differently depending value of @racket[v]:

If @racket[(exn? v)], then @racket[($fail v)] is shown as @racket[(exn->string v)].

If @racket[(string? v)], then @racket[($fail v)] is shown as @racket[v].

Otherwise, @racket[($fail v)] is shown as @racket[(~s v)].
}


@section{Package Messages}

@defstruct*[($output-not-found $message) ([query xiden-query-string?] [output-name string?]) #:prefab]{
A @tech{package output} was not defined in the @tech{package definition} matching @racket[query].
}


@defstruct*[($built-package-output $message) ([name string?] [output-name string?]) #:prefab]{
@project-name successfully processed @racket[(build output-name)] in
the context of a @tech{package definition} module.
}


@defstruct*[($reused-package-output $message) ([name string?] [output-name string?]) #:prefab]{
}


@defstruct*[($undeclared-racket-version $message) ([name string?]) #:prefab]{
}


@defstruct*[($package-malformed $message) ([name string?] [errors (listof string?)]) #:prefab]{
}


@section{CLI Messages}

@defstruct*[($cli $message) () #:prefab]{
A message that pertains to the command line interface.
}

@defstruct*[($cli:undefined-command $cli) ([command string?])  #:prefab]{
A user passed @racket[command] as a subcommand, but it is not defined.
}

@defstruct*[($invalid-workspace-envvar $message) () #:prefab]{
@racket[(getenv "XIDEN_WORKSPACE")] is not useable as a @tech{workspace} path.
}

@section{Transfer Messages}

@defstruct*[($transfer $message) ([name string?]) #:prefab]{
Represents a transfer status with a given @racket[name]. The name is
derived from a @tech{package input}'s name, or the name of a source
used for that input.
}

@defstruct*[($transfer-progress $transfer) ([bytes-read exact-nonnegative-integer?] [max-size (or/c +inf.0 exact-positive-integer?)] [timestamp exact-positive-integer?]) #:prefab]{
Represents progress transferring bytes to a local source.

Unless @racket[max-size] is @racket[+inf.0], @racket[(/ bytes-read
max-size)] approaches @racket[1].  You can use this along with the
@racket[timestamp] (in seconds) to reactively compute an estimated
time to complete.
}


@defstruct*[($transfer-small-budget $transfer) () #:prefab]{
A request to transfer bytes was rejected because the user does not
allow downloads of a required size.  This message also applies if a
transfer cannot estimate the number bytes to read, and the user does
not allow unlimited transfers.

See @racket[XIDEN_FETCH_TOTAL_SIZE_MB] and @racket[XIDEN_FETCH_PKGDEF_SIZE_MB].
}


@defstruct*[($transfer-over-budget $message) ([size exact-positive-integer?]) #:prefab]{
A request to transfer bytes was halted because the transfer read more
than @racket[size] bytes, which the user's configuration forbids.

See @racket[XIDEN_FETCH_TOTAL_SIZE_MB] and @racket[XIDEN_FETCH_PKGDEF_SIZE_MB].
}


@defstruct*[($transfer-timeout $message) ([bytes-read exact-nonnegative-integer?]) #:prefab]{
A request to transfer bytes was halted after @racket[bytes-read] bytes
because no more bytes were available after so much time.

See @racket[XIDEN_FETCH_TIMEOUT_MS].
}


@defstruct*[($unsupported-racket-version $message) ([name string?] [versions (listof string?)]) #:prefab]{
}

@defstruct*[($source-fetched $message) ([source-name string?] [fetch-name string?]) #:prefab]{
}

@defstruct*[($fetch-failure $message) ([name string?]) #:prefab]{
}

@defstruct*[($source-method-ruled-out $message) ([source-name string?] [fetch-name string?] [method-name string?] [reason string?]) #:prefab]{
}

@defstruct*[($unverified-host $message) ([url string?]) #:prefab]{
GET @racket[url] failed because the host did not pass authentication using HTTPS.

See @racket[XIDEN_TRUST_UNVERIFIED_HOST].
}


@section{Signature Checking}

@defstruct*[($signature-status $message) ([input-name string?] [input-source string?]) #:prefab]{
A message pertaining to a signature check on a @tech{package input} named
@racket[input-name].  The bytes for the input came from @racket[input-source].
}

@defstruct*[($signature-unchecked $signature-status) () #:prefab]{
A @tech{package input} transferred from a given @racket[source] skipped
the signature check.
}

@defstruct*[($signature-distrust-public-key $signature-status) ([public-key-path path-string?]) #:prefab]{
A @tech{package input} was rejected because the user did not trust the
public key located at @racket[public-key-path].

See @racket[XIDEN_TRUST_ANY_PUBLIC_KEY] and @racket[XIDEN_TRUSTED_PUBLIC_KEYS].
}

@defstruct*[($signature-trust-unsigned $message) ([name string?] [source string?]) #:prefab]{
}

@defstruct*[($signature-verified $message) ([name string?] [source string?]) #:prefab]{

}

@defstruct*[($signature-mismatch $message) ([name string?] [source string?]) #:prefab]{
A @tech{package input} was rejected when transferred from a given
@racket[source].  The reason being that the signature on the input
failed verification with the associated public key.

See @racket[XIDEN_TRUST_BAD_SIGNATURE].
}

@defstruct*[($signature-missing $message) ([source string?]) #:prefab]{
A @tech{package input} was rejected when transferred from a given
@racket[source].  The reason being that there was no signature declared
with the input.

See @racket[XIDEN_TRUST_UNSIGNED].
}
