#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    denxi/output
                    denxi/monad
                    denxi/subprogram
                    denxi/string
                    @except-in[denxi/pkgdef #%module-begin]]
         "../../shared.rkt"]

@title{Package Outputs}

@defmodule[denxi/output]

@defstruct*[package-output ([name string?] [steps list?] [make-subprogram (-> subprogram?)])]{
A @deftech{package output} is an instance of @racket[package-output].
Each instance represents a named subprogram that claims to follow the
instructions shown in @racket[steps].

Specifically, @racket[steps] is a list of syntax objects or data that
are suitable for use in @racket[mdo].

@racket[make-subprogram] returns a subprogram that creates files. The
subprogram does not need to be atomic, but it should mirror the
content of @racket[steps].
}

@defproc[(find-package-output [name string?] [steps (listof package-output?)])
                              (or/c #f package-output?)]{
Like @racket[findf], except the input list must consist of
@tech{package outputs}, and the search will only compare names.
}


@defproc[(encode-package-output [to-encode package-output?]) list?]{
@margin-note{Any output created using @racket[output] in the
@racketmodname[denxi] or @racketmodname[denxi/pkgdef] languages are
suitable for use with @racket[encode-package-output].}
Returns a quoted @racket[output] term suitable for use in a
@tech{package definition}. The term will use the elements of
@racket[(package-output-steps to-encode)], which means the correctness
of the returned list depends on if @racket[(package-output-steps
to-encode)] and @racket[(package-output-make-subprogram to-encode)] are
equivalent.
}

@defform[(transparent-package-output name step ...)
         #:contracts ([name non-empty-string?])]{
Expands to a new @racket[package-output] where the
@racket[package-output-steps] and
@racket[package-output-make-subprogram] fields both use the provided
sequence of @racketid[step]s.

Specifically, @racket[package-output-steps] is @racket[(quote (mdo
step ...))]  and @racket[package-output-make-subprogram] is
@racket[(lambda () (mdo step ...))].
}
