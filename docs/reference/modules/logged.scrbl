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

@defform[(logged/c contract-expr)]{
Produces a contract for a logged procedure. The procedure must return
a value matching @racket[contract-expr] as the first value.
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

@section{Logged Program Control}

@defform[(define-logged (id formals ...) body ...)]{
Like @racket[(define (id formals ...) body ...)], except the procedure
runs in a continuation with the following injected procedure bindings:

@itemlist[
@item{@racket[($use v [messages])]: Aborts the program with @racket[v] as the result and the given message log.
@racketid[messages] defaults to the current program log.}

@item{@racket[($pass [msg])]: Abort the program with a @racket[SUCCESS] result and an optional new message. If no message is passed, then the program log is unaffected.}

@item{@racket[($fail [msg])]: Abort the program with a @racket[FAILURE] result and an optional new message. If no message is passed, then the program log is unaffected.}

@item{@racket[($run! l)]: Equivalent to @racket[(run-log l m)], where @racket[m] is bound to current program log.}
]

The following example defines two equivalent procedures that clarify
how @racket[define-logged] reduces code volume.

@racketblock[
(define-logged (interpret variant)
  (cond [(eq? 'ok variant)
         ($pass ($show-string "Result is okay"))]
        [(eq? 'no variant)
         ($fail ($show-string "Result is not okay"))]
        [(logged? variant)
         (call-with-values ($run! variant) $use)]))

(define (interpret result)
  (logged
   (lambda (messages)
     (call/cc
       (lambda (return)
         (cond [(eq? 'ok variant)
                (return SUCCESS ($show-string "Result is okay"))]
               [(eq? 'no variant)
                (return FAILURE ($show-string "Result is not okay"))]
               [(logged? variant)
                (call-with-values (run-log variant messages) return)]))))))
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

@section{Testing Logged Procedures}

@defmodule[(submod xiden/logged test)]

@defproc[(test-logged-procedure #:with [initial null] msg l p) void?]{

Equivalent to

@racketblock[
(test-case msg (call-with-values (λ () (run-log l initial)) p))
]
}
