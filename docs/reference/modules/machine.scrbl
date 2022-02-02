#lang scribble/manual

@require["../../shared.rkt"
         @for-label[racket/base
		    racket/contract
                    denxi/machine]]

@title{Abstract Machine}

@defmodule[denxi/machine]

@racketmodname[denxi/machine] defines a modified variant of a Turing
machine. Each machine has a fixed head that only ever transforms a
non-empty list to a list of at least the same length by functional
update. The list acts as the machine's state.

The @racket[car] of the state represents the current value computed by
the machine. If this value is @racket[halt], then the machine will not
transition to a new state.

The @racket[cdr] of the state contains only @tech{messages}, ordered
such that the most recent message comes first.  This represents
contextual information leading up to the current value.


@deftogether[(
@defthing[halt symbol?]
@defthing[state-undefined state-undefined?]
@defstruct*[machine ([procedure (-> (state/c any/c) (state/c any/c))]) #:transparent]
)]{
A @deftech{machine} is an implementation of @racket[gen:monad], and a
value returned by @racket[machine]. An instance of @racket[machine]
contains a Racket procedure that programs a single state transition.

When applying the procedure, any @racket[(negate exn:break?)] value
raised will be caught for use in @racket[machine-halt-with].

@racket[machine] implements @racket[prop:procedure]. The machine
instance accepts an optional state argument.  When no arguments are
provided, the state is @racket[state-undefined]. If the state uses the
@tech/reference{uninterned} @tech/reference{symbol} @racket[halt] as a
value, the machines will return the state without calling
@racket[procedure].

@racketblock[
(define m
  (machine
    (lambda (state)
      (state-set-value (state-add-message state ($show-string "Putting 2 and 2 together"))
        (+ 2 2)))))

(equal? (m) (list 4 ($show-string "Putting 2 and 2 together")))
(equal? (m (list halt)) (list halt))
]


@defproc[(machine-unit [v any/c]) machine?]{
Returns a @tech{machine} that sets @racket[v] as the new value for a
state, with no added messages.
}

@defproc[(machine/c [domain/c contract?] [range/c contract?]) contract?]{
Returns a @tech/reference{contract} for a @tech{machine} that
transitions a state from one value type to another.
}


@defproc[(machine-bind [m machine?] [f (-> any/c machine?)]) machine?]{
Returns a @tech{machine} that computes another machine in terms of a
state value. Used as a monadic bind operation.
}


@defproc[(machine-halt-with [v any/c]) machine?]{
Returns a @tech{machine} that sets a halt state.

The new state's messages will include @racket[(coerce-$message v)].
}


@defproc[(machine-coerce [v any/c]) machine?]{
Equivalent to @racket[(if (machine? v) v (machine-unit v))]
}


@deftogether[(
@defproc[(machine-acyclic [key any/c] [proc (-> (state/c any/c) (state/c any/c))]) machine?]
@defstruct*[($cycle $message) ([key any/c])]
)]{
Returns a @tech{machine} with cycle detection enabled for
@racket[proc].  If @racket[proc] is ever called recursively when an
internal continuation mark set contains @racket[key], then the machine
halts with @racket[($cycle key)].
}


@defthing[state-like? predicate/c]{
Returns @racket[#t] if the argument a non-empty list, without
checking individual elements.
}

@defthing[state-halt? predicate/c]{
Returns @racket[#t] if the argument is a state with @racket[halt] as
the current value.
}

@defthing[state-undefined? predicate/c]{
Returns @racket[#t] if the argument is a state with @racket[undefined]
as the current value.
}

@defproc[(state-halt-with [state state-like?] [v any/c]) state-halt?]{
Returns a new halt state including @racket[(coerce-$message v)] as a message.
}

@defproc[(state-add-message [state state-like?] [msg $message?]) state-like?]{
Returns a state with @racket[msg] as an additional message.
}

@defproc[(state-set-value [state state-like?] [v any/c]) state-like?]{
Returns a state with @racket[v] as a new value.
}

@defproc[(state/c [value/c contract?]) contract?]{
Returns a @tech/reference{contract} for a @tech{machine} state with a
specific value type.
}


@section{Testing Machines}

@defmodule[(submod denxi/machine test)]

@defform[(check-machine-state m pattern)]{
Checks if the state returned by @racket[(m)] matches @racket[pattern], according to @racket[match].
}

@defform[(check-machine-value m pattern)]{
Checks if @racket[(car (m))] matches @racket[pattern], according to @racket[match].
}

@defform[(check-machine-messages m . patterns)]{
Checks if @racket[(cdr (m))] matches @racket[(list . patterns)],
according to @racket[match].
}

@defform[(check-machine-halt m . patterns)]{
Like @racket[check-machine-messages], where the state's value must be @racket[halt].
}

@defproc[(check-machine-contract-violation [domain/c contract?] [range/c contract?] [m machine?] [s (state/c any/c)]) void?]{
Check if @racket[(m s)] violates @racket[(machine/c domain/c range/c)].
}
