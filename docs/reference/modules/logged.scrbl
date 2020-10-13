#lang scribble/manual

@require["../../shared.rkt" @for-label[racket/base racket/contract racket/function xiden/logged xiden/message]]

@title{Logged Programs}

@defmodule[xiden/logged]

A @deftech{logged procedure} is an instance of the monadic value type
@racket[logged]. A @tech{logged procedure} is a procedure that returns
some value and related @tech{messages}. A program composed of
@tech{logged procedures} eventually terminates with a complete
@tech{message} log along with a @racket[SUCCESS] or @racket[FAILURE]
value.


@section{Fundamentals}

@defthing[messy-log/c contract? #:value (or/c $message? (listof (recursive-contract messy-log/c)))]{
A @deftech{messy log} is a @tech{message} or an arbitrarily-nested
list where @tech{messages} are the only non-list elements.

This contract is not used in some parts of the implementation for
performance reasons, but will be cited in this reference for
clarification reasons.
}

@defstruct*[logged ([thnk (-> (listof $message?) (values any/c (listof messy-log/c)))]) #:transparent]{
A monadic type that represents a computed value alongside @tech{messages}.
The @racket[thnk] must accept a list of currently accumulated messages, then
perform planned work that may add at least zero new messages.

The latest message must come first in the resulting list, so
@racket[cons] is appropriate for adding a new message.

@racketblock[
(logged (lambda (messages)
  (values (+ 2 2)
          (cons ($show-string "Putting 2 and 2 together")
                messages))))
]

If a computation must halt, use @racket[SUCCESS] or @racket[FAILURE]
as the computed value.

@racketblock[
(logged (lambda (messages)
  (values FAILURE
          (cons ($show-string "I can't go on!")
                messages))))
]

It's fine to @racket[cons] another list of messages onto an existing
list. This creates a @tech{messy log}.

@racketblock[
(logged (lambda (messages)
  (values whatever
          (cons (list ($show-string ...) ($show-datum ...))
                messages))))
]
}

@section{Terminal Values}

@deftogether[(
@defthing[SUCCESS symbol?]
@defthing[FAILURE symbol?]
)]{
When using an instance of @racket[logged], evaluation stops at the
moment one of these @tech/reference{uninterned}
@tech/reference{symbols} are encountered.

@project-name uses these values to distinguish between Racket booleans
and values that control @racket[logged] evaluation.
}

@section{Alternative Constructors}

@defproc[(logged-unit [v any/c]) logged?]{
Returns a @racket[logged] instance that yields @racket[v] as a computed value, with no added messages.
}

@defproc[(logged-failure [variant any/c]) logged?]{
Returns @racket[(logged (λ (m) (values FAILURE (cons V messages))))],
where @racket[V] is

@itemlist[
@item{@racket[($show-string (exn->string variant))], if @racket[(exn? variant)] is true.}
@item{@racket[variant], if @racket[($message? variant)] is true.}
@item{@racket[($show-datum variant)] in all other cases.}
]
}

@defproc[(logged-attachment [v any/c] [next (or/c $message? (listof $message?))]) logged?]{
Returns @racket[(logged (λ (m) (values v (cons next m))))].
}

@defproc[(logged-map [f (-> $message? $message?)] [to-map logged?]) logged?]{
Returns a new @racket[logged] instance such that each message produced
by @racket[to-map] is included in the combined log using @racket[(map
f (run-log to-map null))].

Use this to “scope” messages.

@racketblock[
(define-message $build-log-entry (name message))

(code:comment "hypothetical")
(define (create-build) (logged (lambda (messages) ...)))

(define build
  (logged-map (curry $build-log-entry "my-build")
              (create-build)))
]

}

@section{Entry Points for Logged Programs}

@defproc[(run-log [program logged?] [messages (listof $message?) null]) (values any/c (listof $message?))]{
Applies all delayed work in @racket[program].  Returns a value and a
list of @tech{messages} representing log output.
}


@defproc[(get-log [program logged?]) (listof $message?)]{
Like @racket[run-log], but returns only the list of messages attached to the computed value.

Additionally, that list is @racket[flatten]ed, then @racket[reverse]d.
}
