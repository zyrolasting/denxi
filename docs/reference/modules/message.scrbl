#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/match
                    xiden/logged
                    xiden/format
                    xiden/message
                    xiden/rc
                    xiden/port]
         "../../shared.rkt"]

@title{Messages}

@defmodule[xiden/message]

A @deftech{message} is an instance of the @racket[$message]
@tech/reference{prefab} @tech/reference{structure} used to share
information in the Xiden runtime, in logs, and between
processes.  When the term “@tech{message}” is ambiguous, then
@deftech{Xiden message} applies the context of this section.

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
Xiden instead composes messages into machine-readable
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
