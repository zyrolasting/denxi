#lang scribble/manual

@require["../../shared.rkt"
         @for-label[racket/base
                    racket/contract
                    racket/function
                    racket/list
                    xiden/subprogram
                    xiden/message
                    xiden/monad]]

@title{Subprograms}

@defmodule[xiden/subprogram]

A @deftech{subprogram} in the context of Xiden is an instance of the
monadic value type @racket[subprogram]. An instance of
@racket[subprogram] contains a Racket procedure that returns a value
and some @tech{messages} representing a @tech{subprogram log}.  When a
composition of subprograms execute, a special @racket[FAILURE] value
returned by one subprogram prevents execution of following
subprograms.

This control structure is necessary because subprograms are supposed
to operate strictly under functional composition in @racket[mdo]. Any
unhandled exceptions raised during subprogram evaluation are
understood under the context of @racket[FAILURE].


@section{Fundamentals}

@defthing[subprogram-log/c contract? #:value
                           (listof (or/c $message?
                                         (recursive-contract subprogram-log/c)))]{
A @deftech{subprogram log} is a list of any structure where
@tech{messages} are the only non-list elements. Unlike a @tech{program
log}, subprogram logs are “messy” due to being actively under
construction. No structure is imposed on the list other than the first
@tech{message} of any list (no matter how nested) in a subprogram log
represents the most recent activity.

This contract is not used in some parts of the implementation for
performance reasons, but will be cited in this reference for
clarification reasons.
}


@defstruct*[subprogram ([thnk (-> (listof $message?) (values any/c subprogram-log/c))]) #:transparent]{
A monadic type that computes a value alongside a @tech{subprogram
log}.  The @racket[thnk] must accept a @tech{subprogram log}
representing prior program activity, then perform planned work that
may add at least zero new messages.

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

It's fine to @racket[cons] another list of messages onto a
@tech{subprogram log}. It is only important that a message
representing the most recent activity comes first should the list be
flattened.

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


@defthing[FAILURE symbol?]{
An @tech/reference{uninterned} @tech/reference{symbol} that, when
returned by a subprogram, prevents evaluation of a subsequent
subprogram.
}


@section{Alternative Constructors}

@defproc[(subprogram-unit [v any/c]) subprogram?]{
Returns a @racket[subprogram] instance that yields @racket[v] as a
computed value, with no added messages.
}


@defproc[(subprogram-failure [variant any/c]) subprogram?]{
Returns @racket[(subprogram (λ (m) (values FAILURE (cons V
messages))))], where @racket[V] is @racket[variant] if
@racket[($message? variant)] is true.  Otherwise, @racket[V] is
@racket[($show-string (exn->string variant))].
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
@defproc[(subprogram-acyclic [key any/c] [proc (-> (listof $message?) (values any/c subprogram-log/c))]) subprogram?]
@defstruct*[($cycle $message) ([key any/c])]
)]{
@racket[subprogram-acyclic] behaves like @racket[(subprogram proc)] with cycle
detection.  If another @racket[subprogram] instance runs in the context of
@racket[proc], and that instance was constructed using
@racket[subprogram-acyclic] and a value @racket[equal?] to @racket[key],
then evaluation ends early. In that case, the computed value is
@racket[FAILURE] and @racket[($cycle key)] appears in the log.
}


@section{Subprogram Control}

@defform[(define-subprogram (id formals ...) body ...)]{
Like @racket[(define (id formals ...) body ...)], except the procedure
runs in a continuation with the following injected procedure bindings:

@itemlist[
@item{@racket[($use v [messages])]: Aborts the program with @racket[v] as the result and the given message log.
@racketid[messages] defaults to the current @tech{subprogram log}.}

@item{@racket[($fail [msg])]: Abort the program with a @racket[FAILURE] result and an optional new message. If no message is passed, then the program log is unaffected.}

@item{@racket[($attach v [msg])]: Abort the program with @racket[v] as the result and an optional new message. If no message is passed, then the program log is unaffected.}

@item{@racket[($run! l)]: Equivalent to @racket[(run-subprogram l m)], where @racket[m] is bound to current subprogram log.}

@item{@racket[$messages]: A reference to the current subprogram log.}
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
Returns a @tech{subprogram} that applies @racket[dump-message] to
every element of the @racket[preamble], then every element in the
current @tech{subprogram log}. The subprogram will use @racket[value]
as the result.
}


@defproc[(subprogram-branch [test subprogram?] [on-failure subprogram?]) subprogram?]{
Returns a @tech{subprogram}. If the @racket[test] program fails, then
the result of the returned program depends on @racket[on-failure].

All @tech{messages} are collected in the @tech{subprogram log}.
}

@defproc[(subprogram-fold [initial subprogram?]
                          [fns (listof (-> any/c subprogram?))])
                          subprogram?]{
Returns a @tech{subprogram}.

Bind each function in @racket[fns] (under the context of
@racket[subprogram-bind]) starting from @racket[initial].

Assuming @racketid[fn_0] is the first element of @racket[fns], and
@racketid[fn_N] is the last element of @racket[fns], the returned
subprogram behaves like the following @racket[mdo] form.

@racketblock[
(mdo v_0 := initial
     v_1 := (fn_N v_0)
     v_2 := (fn_N-1 v_1)
     ...
     v_N-1 := (fn_0 v_N-2)
     v_N := (fn_0 v_N-1))
]

Notice that @racket[fns] are applied in reverse, so the last element
of @racket[fns] is the first to operate on the output of
@racket[initial].

An example follows. Also note that the literal @tech{subprogram log}
near the end appears to follow the order of the input functions.
Since message logs are assembled using @racket[cons], the fact the
functions were applied in reverse made the messages also appear in
reverse.

@racketblock[
(define ((shift amount) accum)
  (subprogram (lambda (messages)
    (values (+ amount accum)
            (cons ($show-datum amount)
                  messages)))))

(define sub
  (subprogram-fold (subprogram-unit 0)
                   (list (shift -1)
                         (shift 8)
                         (shift -3))))

(define-values (result messages)
  (run-subprogram sub))

(equal? result 4)
(equal? messages
        (list ($show-datum -1)
              ($show-datum 8)
              ($show-datum -3)))
]
}


@section{Entry Points for Subprograms}

@defproc[(run-subprogram [program subprogram?] [messages subprogram-log/c null]) (values any/c (listof $message?))]{
Applies all delayed work in @racket[program].  Returns a value and an
updated @tech{subprogram log}.

When applying the procedure in the given subprogram, any
@racket[(negate exn:break?)] @italic{value} @racketid[V] (not just
exception types) raised as an exception will be caught. In that case,
the invocation of @racket[run-subprogram] that caught the value will
return @racket[(run-subprogram (subprogram-failure V) messages)].
}


@defproc[(get-subprogram-log [program subprogram?]) (listof $message?)]{
Like @racket[run-subprogram], but returns only the list of messages
attached to the computed value.

Additionally, that list is @racket[flatten]ed, then @racket[reverse]d.
}

@defproc[(get-subprogram-value [program subprogram?]) any/c]{
Like @racket[run-subprogram], but returns only the computed value.
}


@section{Testing Subprograms}

@defmodule[(submod xiden/subprogram test)]

@defproc[(test-subprogram [#:with initial (listof $message?) null]
                          [test-message string?]
                          [subprogram-procedure subprogram?]
                          [continue procedure?])
                          void?]{
Equivalent to a unit test case with the given @racket[test-message], where the
test evaluates @racketblock[(call-with-values (λ () (run-subprogram subprogram-procedure initial)) continue)].
}
