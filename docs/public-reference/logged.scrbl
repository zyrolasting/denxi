#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base racket/contract xiden/logged xiden/message]]

@title{Monadic Control}

@defmodule[xiden/logged]

@project-name uses monadic operations to attach @tech{messages} to
values and control program flow. The monadic value type is
@racket[logged], which represents a delayed program that eventually
terminates with a @tech{message} log.

@racket[logged] instances act like a hybrid of the State and Maybe
monads in Haskell. The State-like aspect adds messages to a log and
adds room for a computed value. The Maybe-like aspect short-circuits
evaluation if a special value appears.


@section{Fundamentals}

@defstruct*[logged ([thnk (-> (listof $message?) (values any/c (listof $message?)))]) #:transparent]{
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
list, since @racket[get-log] can clean up the result.

@racketblock[
(logged (lambda (messages)
  (values whatever
          (cons (list ($show-string ...) ($show-datum ...))
                messages))))
]
}

@defthing[SUCCESS symbol?]{
Represents a finished program with a desired result.
}

@defthing[FAILURE symbol?]{
Represents a finished program with a undesired result.
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


@section{Entry Points for Logged Programs}

@defproc[(run-log [program logged?] [messages (listof $message?) null]) (values any/c (listof $message?))]{
Applies all delayed work in @racket[program].  Returns a value and a
list of @tech{messages} representing log output.
}


@defproc[(get-log [program logged?]) (listof $message?)]{
Like @racket[run-log], but returns only the list of messages attached to the computed value.

Additionally, that list is @racket[flatten]ed, then @racket[reverse]d.
}