#lang denxi/document

@title{Artifacts}

@defmodule[denxi/artifact]

@defstruct*[artifact ([source source-variant?]
                      [integrity (or/c #f well-formed-integrity?)]
                      [signature (or/c #f well-formed-signature?)])]{
An @deftech{artifact} is an instance of @racket[artifact].  Each
instance provides a @tech{source} and the means to verify the
integrity and sender of the bytes produced when the source is
@tech{tapped}.
}

@defproc[(make-artifact [source source-variant?]
                        [int (or/c #f well-formed-integrity?) #f]
                        [sig (or/c #f well-formed-signature?) #f])
                        artifact?]{
A constructor for @racket[artifact] with optional arguments.
}


@defproc[(study-artifact [subject artifact?]
                         [mind mind-implementation/c]
                         [policy transfer-policy?])
                         (subprogram/c known-implementation/c)]{
Returns a subprogram used for its effect.

When the subprogram runs, it attempts to create a @tech{known} in the
@tech{mind} using bytes found in the @tech{artifact}. Data transfer is
governed using @racket[policy].

The bytes are verified and authenticated according to the information
in the artifact.
}


@defproc[(verify-artifact [arti artifact?] [pathrec path-record?])
         (subprogram/c void?)]{
Returns a @tech{subprogram} that fails in the event an @tech{artifact}
does not meet the restrictions set by the @tech{runtime
configuration}. In that case, the @tech{subprogram log} will contain
any relevant @tech{messages} explaining a verification failure.

The computed value of the subprogram is @racket[(void)] because the
value is not important. @racket[verify-artifact] is used for its
ability to halt @tech{subprograms} when an artifact fails
verification.
}


@defproc[(lock-artifact [arti artifact?]
                        [exhaust exhaust/c raise]
                        [#:content? content? any/c #t]
                        [#:integrity? integrity? any/c #t]
                        [#:signature? signature? any/c #t]
                        [#:content-budget content-budget budget/c (* 1024 50)]
                        [#:digest-budget digest-budget budget/c +inf.0]
                        [#:public-key-budget public-key-budget budget/c +inf.0]
                        [#:signature-budget signature-budget budget/c +inf.0])
                        artifact?]{
Returns a functionally-updated @racket[artifact].

When @racket[content?] is a true value, then the
@racket[artifact-source] field @racketid[C] is replaced by

@racketblock[
(lock-source C content-budget exhaust)
]

When @racket[integrity?] is a true value, then the
@racket[artifact-integrity] field @racketid[I] is replaced by

@racketblock[
(lock-integrity #:digest-budget digest-budget
                exhaust)
]

When @racket[signature?] is a true value, then the @racket[artifact-signature]
field @racketid[S] is replaced by

@racketblock[
(lock-signature #:public-key-budget public-key-budget
                #:signature-budget signature-budget
                S exhaust)
]
}


@defstruct*[($artifact $message) () #:prefab]{
A @tech{message} pertaining to an artifact.
}

@defstruct*[($artifact:integrity $artifact)
            ([status symbol?]
             [chf-symbol (or/c #f symbol?)]) #:prefab]{
Shows the status of an integrity check performed on an artifact.

@racket[status] is a value returned from @racket[check-integrity].

@racket[chf-symbol] is the symbolic name of the CHF used in the
integrity check, or @racket[#f] if the CHF was missing.
}

@defstruct*[($artifact:signature $artifact)
            ([status symbol?]
             [public-key (or/c #f bytes?)]) #:prefab]{
Shows the status of a signature verification performed on an artifact.

@racket[status] is a value returned from @racket[check-signature].

@racket[public-key] is the unencoded bytes of the public key used to
verify the signature, or @racket[#f] if the public key was missing.
}
