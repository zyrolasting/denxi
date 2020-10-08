#lang scribble/manual

@title{Command Line Utilities}

@defmodule[xiden/cmdline]

@racketmodname[xiden/cmdline] provides all bindings from
@racketmodname[racket/cmdline], as well as the bindings documented in
this section. Unlike @racketmodname[xiden/cli],
@racketmodname[xiden/cmdline] does not actually define a command line
interface. It only helps @racketmodname[xiden/cli] do so.

@defthing[exit-code/c flat-contract? (integer-in 0 255)]{
An exit code for use in an @tech/reference{exit handler}.
}

@defthing[arguments/c flat-contract? (or/c (listof string?) (listof vector?))]{
Represents arguments provided in a command line. An argument list may
be a vector or list of strings for flexibility reasons.
}

@defproc[(entry-point [args arguments/c] [formatter message-formatter/c] [handler (-> arguments/c (-> exit-code/c (or/c $message? list?) any) (values exit-code/c (or/c $message? (listof $message?))))]) exit-code/c]{

}


@defform[(with-rc flags body ...)]{
Like @racket[begin], except the body is evaluated in a
@tech/reference{parameterization} of all @tech{settings} derived from
@racket[flags]--a value computed by @racket[run-command-line].


}
