#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/fasl
                    racket/match
                    racket/pretty
                    racket/serialize
                    xiden/l10n
                    xiden/message
                    xiden/port
                    xiden/printer]
         "../../shared.rkt"]

@title{Messages}

@defmodule[xiden/message]

A @deftech{message} is an instance of the @racket[$message]
@tech/reference{prefab} @tech/reference{structure} used to share
information in the @project-name runtime, in logs, and between
processes.  When the term “@tech{message}” is ambiguous, then
@deftech{@project-name message} applies the context of this section.

@tech{Message} types may form a heirarchy using colon-separated
identifiers that start with @racket[$]. The @racket[$message] type
itself has no semantics beyond serving as the root type, and its
identifier does not appear in other structure type identifiers. For
example, identifiers pertaining to command line messages start with
@tt{$cli}, not @tt{$message:cli}.

The type heirarchy serves only to organize statements under a given
topic. Meaning that a @racket[$transfer:budget?] would capture
messages pertaining to a budget for a byte transfer, but an instance
of @racket[$transfer:budget:exceeded] is an exact statement reporting
an overrun.

A message may require another message to serve as context.  A type
heirarchy is unhelpful for this purpose, since
@racket[$transfer:budget:exceeded] could apply to a download in the
context of a build process or a verification process. It is difficult
to define a single type to capture this kind of variation, so
@project-name instead composes messages into machine-readable
documents.


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

@defstruct*[($regarding $message) ([subject $message?] [body $message?]) #:prefab]{
Represents a request to show one message in the context of another.
}


@defproc[(scope-message [m $message?] [scope (listof $message?) (get-message-scope)]) $message?]{
Returns @racket[m] if @racket[scope] is @racket[null].

Otherwise, returns @racket[(scope-message ($regarding (car scope) m) (cdr scope))].

Use to wrap a @tech{message} with @racket[$regarding], where the exact
structure of the output may depend on the current
@tech/reference{dynamic extent}.
}


@defproc[(call-in-message-scope [m $message] [proc (-> any)]) any]{
Equivalent to @racket[(call-in-message-scope* (cons m (get-message-scope)) proc)].
}


@defproc[(call-in-message-scope* [ms (listof $message?)] [proc (-> any)]) any]{
Returns @racket[(proc)]. While @racket[proc] has control,
@racket[(get-message-scope)] is @racket[eq?] to @racket[ms].
}


@defform[(in-message-scope m body ...)]{
Expands to @racket[(call-in-message-scope m (lambda () body ...))].
}

@defproc[(get-message-scope) (listof $message?)]{
Returns a list of @tech{messages} representing a dynamic scope
for other messages. See @racket[call-in-message-scope].
}


@section{Printing Messages}

@defmodule[xiden/printer]

@defstruct*[($verbose $message) ([message $message?]) #:prefab]{
A wrapper for a message that only appears to a user if
@racket[(XIDEN_VERBOSE)] is @racket[#t].
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
