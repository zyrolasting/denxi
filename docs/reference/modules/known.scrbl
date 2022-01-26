#lang scribble/manual

@require[@for-label[racket
                    denxi/known
		    denxi/monad
		    denxi/subprogram]
         "../../shared.rkt"]

@title{Knowns}

A @deftech{known} is an implementation of the @racket[gen:known]
@tech/reference{generic interface}, and of the semantics defined
in this section.


@section{Known Semantics}

The word “known” is used here in noun form to refer to an
implementation of @racket[gen:known].  The noun form is also prominent
in source code, such as in @racket[(define known (know))].

These semantics encode a past participle use, specifically “here is
something @italic{known} by names.” Equivocation is unavoidable here.

A known associates a byte string (“datum”) with a list of strings
(“names”). Either are subject to functional update.

Names are encoded as Racket
string lists to force them into memory. This avoids infinite sequences
or streams. Forcing all names into memory discourages use of
unnecessary characters.

Each known contains its own namespace, so no two knowns may collide.
Names must be strings, because names are artifacts of language. Names
change independently of a datum, as language warrants.

A datum may be stored anywhere. A known guarentees the existence of a
datum, but not its integrity or location.  A known may not fail to
produce at least zero bytes, even if they are of its choosing. A datum
may only be replaced in full, with trust that the datum received is
the last datum written.

Denxi cannot use knowns in security-critical code due to this
trust. Operations on knowns occur in the context of all available
security checks, to simplify the act of associating human language to
concrete data.


@section{Known Generic Interface}

@defthing[gen:known]{
Defines a @tech/reference{generic interface} for @tech{knowns}.
}

@defthing[known? predicate/c]{
Returns @racket[#t] when given a partial or full implementation of
@racket[gen:known].
}

@defproc[(known-get-names [k known?]) (subprogram/c (listof string?))]{
Returns a @tech{subprogram} for producing a list of names.

The order of the names does not impact Denxi's invariants.  However,
the lists are subject to linear searches. It may help to order names
by frequency of use.
}


@defproc[(known-put-names [known known?] [names (listof string?)]) (subprogram/c void?)]{
Returns a @tech{subprogram} that changes the value of @racket[(known-get-names known)].
}


@defproc[(known-put-bytes [k known?] [in input-port?]) (subprogram/c void?)]{
Returns a @tech{subprogram} for replacing the value of
@racket[(known-get-bytes k)] with all bytes consumed from @racket[in],
starting from the port's current position.

The subprogram is responsible for shared resources external to the process.
}


@defproc[(known-open-bytes [k known?]) (subprogram/c input-port?)]{
Returns a @tech{subprogram} for opening the byte content of a known
for reading.
}


@section{Derived Known Procedures}

These procedures are not part of @racket[gen:known], but may use the
interface.

@defproc[(know [names (listof string?) null] [data bytes? #""]) known?]{
Return a new @tech{known} that encapsulates zero names and zero bytes,
all in memory.
}

@defproc[(known-get-bytes [k known?]) (subprogram/c bytes?)]{
Returns a @tech{subprogram} for loading the byte content of a known
into memory.

@section{Example}

This example demonstrates put/get semantics using @racket[know].

@racketblock[
(define-syntax-rule (show expr)
  (subprogram-unit (printf "~s: ~a~n" 'expr expr)))

(define k (know))
(define my-bytes #"hello")
(define my-names '("greeting" "salutation"))
(mdo initial-bytes := (known-get-bytes k)
     (known-put-bytes k (open-input-bytes my-bytes))
     new-bytes := (known-get-bytes k)

     (show (equal? #"" initial-bytes))
     (show (equal? my-bytes new-bytes))

     (code:comment "------------------------")

     inital-names := (known-get-names k)
     (known-put-names k my-names)
     new-names := (known-get-names k)

     (show (equal? #"" initial-names))
     (show (equal? my-names new-names)))
]

This hypothetical s-exp copy operation demonstrates a use of
@racket[known-open-bytes].

@racketblock[
(mdo from-bytes := (known-open-bytes really-big)
     (sequence-map writeln (in-port read from-bytes)))
]

}
