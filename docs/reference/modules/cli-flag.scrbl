#lang scribble/manual

@require["../../shared.rkt"
         @for-label[racket/base
                    racket/contract
                    racket/cmdline
                    xiden/cli-flag
                    xiden/setting]]

@title{Command Line Flags}

@defmodule[xiden/cli-flag]

When parsing command line flags, Xiden does not store
user-defined values as a side effect. Instead, it defers binding
values from a command line to @tech{settings}. Once a process
is ready to determine a @tech{runtime configuration}, it binds
values from a command line using @racket[call-with-bound-cli-flags].

@bold{In addition to the below, @racketmodname[xiden/cli-flag]
provides many identifiers bound to the elements of
@racket[all-flags].} Each provided identifier matches the available
names of the command line flag, @italic{except} the corresponding
setting id.

@defstruct*[cli-flag ([setting setting?]
                      [kind (or/c 'once-each 'once-any 'multi)]
                      [additional-flag-strings (listof string?)]
                      [arity exact-nonnegative-integer?]
                      [convert procedure?]
                      [help-strings (listof string?)])
                      #:transparent]{
Represents a command line flag.

An instance of this structure corresponds to a suitable flag table
entry in @racket[parse-command-line]. The value of @racket[kind]
has the same meaning as it does in @racket[parse-command-line].

An instance is used to generate deferred bindings for @racket[setting],
such that the strings passed in the command line are turned into a
Racket values for the setting using @racket[convert].

Each instance of @racket[cli-flag] implicitly carries a longform flag
equal to @racket[(format "--~a" (setting-id
setting))]. @racket[additional-flag-strings] holds acceptable flags
recognized by an instance.

@racket[convert] and @racket[help-strings] are related in the same way
as they are in a program using @racket[parse-command-line] (see
example below). However, @racket[arity] describes the number of
formals accepted by @racket[convert] minus the flag
argument. Therefore it must be the case that @racket[(equal?
(procedure-arity convert) (add1 arity))].

The below example shows a setting that holds a list of lists, where
each sublist holds three numbers. The following @racket[cli-flag]
instance describes the corresponding command line flag and the
conversion from user-provided strings to a valid value for the
setting.

@racketblock[
(define-setting TRIPLET_LIST
  (listof (list/c real? real? real?))
  null)

; You can use --TRIPLET_LIST, -t, or --triplet
(define --triplet
  (cli-flag TRIPLET_LIST
            'multi
            '("-t" "--triplet")
            3
            (λ (flag a b c)
              (cons (list (string->number a)
                          (string->number b)
                          (string->number c))
                    (TRIPLET_LIST)))
            '("number" "number" "number")))
]
}


@defstruct*[cli-flag-state ([flag-string string?] [flag-definition cli-flag?] [bind (-> (-> any) any)])
            #:transparent]{
Represents a deferred binding for a setting using @racket[flag-definition].

@racket[flag-string] is bound to the exact flag used by the user.
@racket[bind] applies a given thunk such that the setting in
@racket[flag-definition] is bound to a user-defined value in the
extent of said thunk.

A particular use of @racketid[--triplet] from the earlier example
could generate this state.

@racketblock[
(code:comment "Generated from -t 9 7 22 -t 10 0 0 -t 2 4 5")
(cli-flag-state "-t" --triplet
                (lambda (proc)
                  (TRIPLET_LIST '((2 4 5) (10 0 0) (9 7 22))
                  proc)))
]

}

@defthing[all-flags (listof cli-flag?)]{
A list of all built-in flags.
}

@defproc[(find-cli-flag [s setting?]) (or/c #f cli-flag?)]{
Returns an element of @racket[all-flags] used to defer bindings to
@racket[s], or @racket[#f] if no such flag is defined.
}

@defproc[(make-cli-flag-table [c cli-flag?] ...) list?]{
Returns a list suitable for use as a flag table in @racket[parse-command-line].
}

@defproc[(cli-flag-strings [c cli-flag?]) (non-empty-listof string?)]{
Returns all possible command line flags that reference @racket[c],
including the canonical flag derived from @racket[(cli-flag-setting c)].
}

@defproc[(shortest-cli-flag [c cli-flag?]) string?]{
Returns the shortest element of @racket[(cli-flag-strings c)].
}

@defproc[(format-cli-flags [c cli-flag?]) string?]{
Returns a single @tt{/}-separated string that shows
each element of @racket[(cli-flag-strings c)] in order
of shortest to longest.
}

@defproc[(call-with-bound-cli-flags [flag-states (listof cli-flag-state?)]
                                    [continue (-> any)])
                                    any]{
Applies @racket[continue] in a @tech/reference{parameterization} where
all settings affected by @racket[flag-states] are bound to their
corresponding values.
}
