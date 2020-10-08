#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/fasl
                    racket/match
                    racket/pretty
                    racket/serialize
                    xiden/l10n
                    xiden/message
                    xiden/integrity
                    xiden/printer
                    xiden/rc]
         "../shared.rkt"]

@title{Messages}

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
The base type for all @tech{messages}.
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


@section{Printing Messages}

@defmodule[xiden/printer]

@defstruct*[($verbose $message) ([message $message?]) #:prefab]{
A wrapper for a message that only appears to a user if
@racket[(XIDEN_VERBOSE)] is @racket[#t].
}

@defthing[message-formatter/c chaperone-contract? #:value (-> $message? string?)]{
A @deftech{message formatter} is a procedure that translates a
@tech{message} to a human-readable string.
}

@defform[(message-formatter patts ...)]{
Expands to @racket[(λ (m) (match m patts ...))]
}


@defform[(define-message-formatter id patts ...)]{
Expands to @racket[(define id (message-formatter patts ...))]
}

@defform[(define+provide-message-formatter id patts ...)]{
Expands to

@racketblock[
(begin (provide (contract-out [id message-formatter/c]))
       (define-message-formatter id patts ...))]
}

@defproc[(combine-message-formatters [formatter message-formatter/c] ...) message-formatter/c]{
Returns a @tech{message formatter} that uses each @racket[formatter]
in the order passed.
}

@defproc[(write-message [m $message?] [format-message message-formatter/c] [out output-port? (current-output-port)]) void?]{
Writes a @tech{message} to @racket[out] according to the values of
@racket[(XIDEN_READER_FRIENDLY_OUTPUT)], @racket[(XIDEN_FASL_OUTPUT)],
and @racket[(XIDEN_VERBOSE)].

Given @racket[(and (not (XIDEN_VERBOSE)) ($verbose? m))],
@racket[write-message] does nothing.

Otherwise, @racket[write-message] does the following:

@racketblock[
(let ([to-send (if (XIDEN_READER_FRIENDLY_OUTPUT) m (format-message m))])
  (if (XIDEN_FASL_OUTPUT)
      (s-exp->fasl (serialize to-send) out)
      (if (XIDEN_READER_FRIENDLY_OUTPUT)
          (pretty-write #:newline? #t to-send out)
          (displayln to-send out))))]

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
A package output was not defined in the @tech{package definition} matching @racket[query].
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


@section{Localization}

@defmodule[xiden/l10n]

@racketmodname[xiden/l10n] uses @tech{messages} to communicate with
the user according to the value of @racket[(system-language+country)].
Currently, the only supported locale is @tt{en-US}.

@defproc[(get-message-formatter) message-formatter/c]{
Returns a @tech{message formatter} for translating @tech{messages}
to strings in the user's locale.
}

@defproc[(run+print-log [l logged?]) any/c]{
Returns the first value from @racket[(run-log l)].

Before returning control, each @tech{message} @racketid[M] from
@racket[run-log] is printed using

@racketblock[(write-message M (get-message-formatter) (current-output-port))]
}
