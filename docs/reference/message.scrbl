#lang denxi/document

@title{Messages}
@defmodule[denxi/message]

A @deftech{message} is an instance of the @racket[$message]
@tech/reference{prefab} @tech/reference{structure} type.  A list of
messages is therefore suitable for use with @racket[read] and
@racket[write].

By convention, message type identifiers are start with exactly one
@tt{$} and may contain at least zero @tt{:}s. The colons encode a
heirarchy like @racket[exn] subtypes. The @racket[$message] type
itself has no semantics beyond serving as the root type.

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


@defproc[(coerce-$message [v any/c]) $message?]{
Returns @racket[v], if @racket[v] is a @tech{message}.
Otherwise, returns @racket[($show-string (format-value v))].
}


@defstruct*[($show-datum $message) ([value any/c]) #:prefab]{
Represents a request to show the user a Racket value.
}

@defstruct*[($show-string $message) ([message any/c]) #:prefab]{
Represents a request to show the user a string, particularly in
@racket[display] mode.
}
