#lang scribble/manual

@require["../../shared.rkt"
         @for-label[racket/base
                    racket/contract
                    racket/function
                    xiden/subprogram
                    xiden/message]]

@title{Subprograms}

@defmodule[xiden/subprogram]

A @deftech{subprogram} in the context of Xiden is an instance of the
monadic value type @racket[subprogram]. An instance of
@racket[subprogram] contains a normal Racket procedure that returns
some value along with some @tech{messages} representing a
@tech{subprogram log}. A program composed of @tech{subprograms}
eventually terminates with a complete @tech{message} log along with a
value. When multiple sequences subprograms execute, a special
@racket[FAILURE] value returned by one subprogram halts execution of
following subprograms.


@section{Fundamentals}

@defthing[subprogram-log/c contract? #:value (or/c $message? (listof (recursive-contract subprogram-log/c)))]{
A @deftech{subprogram log} is a @tech{message} or an arbitrarily-nested
list where @tech{messages} are the only non-list elements.

This contract is not used in some parts of the implementation for
performance reasons, but will be cited in this reference for
clarification reasons.
}

@defstruct*[subprogram ([thnk (-> (listof $message?) (values any/c (listof subprogram-log/c)))]) #:transparent]{
A monadic type used to produce computed value alongside
@tech{messages}.  The @racket[thnk] must accept a list of currently
accumulated messages, then perform planned work that may add at least
zero new messages.

The latest message must come first in the resulting list, so
@racket[cons] is appropriate for adding a new message.

@racketblock[
(subprogram (lambda (messages)
  (values (+ 2 2)
          (cons ($show-string "Putting 2 and 2 together")
                messages))))
]

If a computation must halt, use @racket[FAILURE] as the computed
value.

@racketblock[
(subprogram (lambda (messages)
  (values FAILURE
          (cons ($show-string "I can't go on!")
                messages))))
]

It's fine to @racket[cons] another list of messages onto an existing
list. This creates a @tech{subprogram log}.

@racketblock[
(subprogram (lambda (messages)
  (values whatever
          (cons (list ($show-string ...) ($show-datum ...))
                messages))))
]
}

@defform[(subprogram/c contract-expr)]{
Produces a contract for a subprogram. The procedure must return
a value matching @racket[contract-expr] as the first value, unless
that value is @racket[FAILURE].
}

@section{Terminal Values}

@defthing[FAILURE symbol?]{
An @tech/reference{uninterned} @tech/reference{symbols} that, when
returned by a subprogram, prevents evaluation of a subsequent
subprogram.
}


@section{Alternative Constructors}

@defproc[(subprogram-unit [v any/c]) subprogram?]{
Returns a @racket[subprogram] instance that yields @racket[v] as a
computed value, with no added messages.
}

@defproc[(subprogram-failure [variant any/c]) subprogram?]{
Returns @racket[(subprogram (λ (m) (values FAILURE (cons V messages))))],
where @racket[V] is

@itemlist[
@item{@racket[($show-string (exn->string variant))], if @racket[(exn? variant)] is true.}
@item{@racket[variant], if @racket[($message? variant)] is true.}
@item{@racket[($show-datum variant)] in all other cases.}
]
}

@defproc[(subprogram-attachment [v any/c] [next (or/c $message? (listof $message?))]) subprogram?]{
Returns @racket[(subprogram (λ (m) (values v (cons next m))))].
}

@defproc[(subprogram-map [f (-> $message? $message?)] [to-map subprogram?]) subprogram?]{
Returns a new @racket[subprogram] instance such that each message produced
by @racket[to-map] is included in the combined log using @racket[(map
f (run-subprogram to-map null))].

Use this to “scope” messages.

@racketblock[
(define-message $build-subprogram-entry (name message))

(code:comment "hypothetical")
(define (create-build) (subprogram (lambda (messages) ...)))

(define build
  (subprogram-map (curry $build-subprogram-entry "my-build")
              (create-build)))
]
}

@defproc[(coerce-subprogram [v any/c]) subprogram?]{
Equivalent to @racket[(if (subprogram? v) v (subprogram-unit v))]
}


@deftogether[(
@defproc[(subprogram-acyclic [key any/c] [proc (-> (listof $message?) (values any/c (listof subprogram-log/c)))]) subprogram?]
@defstruct*[($cycle $message) ([key any/c])]
)]{
@racket[subprogram-acyclic] behaves like @racket[(subprogram proc)] with cycle
detection.  If another @racket[subprogram] instance runs in the context of
@racket[proc], and that instance was constructed using
@racket[subprogram-acyclic] and a value @racket[equal?] to @racket[key],
then evaluation ends early. In that case, the computed value is
@racket[FAILURE] and @racket[($cycle key)] appears in the log.
}

@section{Logged Program Control}

@defform[(define-subprogram (id formals ...) body ...)]{
Like @racket[(define (id formals ...) body ...)], except the procedure
runs in a continuation with the following injected procedure bindings:

@itemlist[
@item{@racket[($use v [messages])]: Aborts the program with @racket[v] as the result and the given message log.
@racketid[messages] defaults to the current program log.}

@item{@racket[($fail [msg])]: Abort the program with a @racket[FAILURE] result and an optional new message. If no message is passed, then the program log is unaffected.}

@item{@racket[($attach v [msg])]: Abort the program with @racket[v] as the result and an optional new message. If no message is passed, then the program log is unaffected.}

@item{@racket[($run! l)]: Equivalent to @racket[(run-subprogram l m)], where @racket[m] is bound to current program log.}

@item{@racket[$messages]: A reference to the current program log.}

]

The following example defines two equivalent procedures that clarify
how @racket[define-subprogram] reduces code volume.

@racketblock[
(define-subprogram (interpret variant)
  (cond [(eq? 'no variant)
         ($fail ($show-string "Result is not okay"))]
        [(subprogram? variant)
         (call-with-values ($run! variant) $use)]))

(define (interpret result)
  (subprogram
   (lambda (messages)
     (call/cc
       (lambda (return)
         (cond [(eq? 'no variant)
                (return FAILURE ($show-string "Result is not okay"))]
               [(subprogram? variant)
                (call-with-values (run-subprogram variant messages) return)]))))))
]
}


@defproc[(dump-subprogram [#:dump-message dump-message (-> $message? any) writeln]
                   [#:force-value value any/c (void)]
                   [preamble $message?] ...)
                   (subprogram/c any/c)]{
Returns a @tech{subprogram} that applies @racket[dump-message]
to every element of the @racket[preamble], then every element in the
current program log. The subprogram will use @racket[value] as
the result.
}


@section{Entry Points for Logged Programs}

@defproc[(run-subprogram [program subprogram?] [messages (listof $message?) null]) (values any/c (listof $message?))]{
Applies all delayed work in @racket[program].  Returns a value and a
list of @tech{messages} representing log output.
}


@defproc[(get-subprogram-log [program subprogram?]) (listof $message?)]{
Like @racket[run-subprogram], but returns only the list of messages attached to the computed value.

Additionally, that list is @racket[flatten]ed, then @racket[reverse]d.
}

@section{Testing Logged Procedures}

@defmodule[(submod xiden/subprogram test)]

@defproc[(test-subprogram-procedure [#:with initial (listof $message?) null]
                                    [test-message string?]
                                    [subprogram-procedure subprogram?]
                                    [continue procedure?])
                                    void?]{
Equivalent to a unit test case with the given @racket[test-message], where the
test evaluates @racketblock[(call-with-values (λ () (run-subprogram subprogram-procedure initial)) continue)].
}
