#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    xiden/logged
                    xiden/message
                    xiden/system]]

@title{System}

@defmodule[xiden/system]

@racketmodname[xiden/system] extends and reprovides
@racketmodname[racket/system]. It specializes process management in
the context of packages.

@defthing[ALL_OS_SYMS (listof symbol?)]{
A manually maintained list of every known value returned from @racket[(system-type 'os)].
}

@defidform[#:kind "syntax-class" os-sym]{
A syntax class that matches members of @racket[ALL_OS_SYMS].
}

@defproc[(run [#:expected-exit-codes expected-exit-codes (listof (integer-in 0 255)) '(0)]
              [#:timeout timeout exact-positive-integer? (* 60 60)]
              [#:stdin stdin (or/c #f input-port?) #f]
              [#:fail-on-stderr? fail-on-stderr? boolean? #t]
              [#:controller controller any/c private]
              [#:cwd wd path-string? (current-directory)]
              [command path-string?]
              [arguments (or/c path-string? string-no-nuls? bytes-no-nuls?)] ...)
              logged?]{
Returns a @tech{logged procedure} @racketid[P] called for its effect.

@racketid[P] creates a subprocess with the given @racket[command] and
@racket[arguments], with @racket[wd] set as the working directory.
@racketid[P] will ensure the subprocess finishes before returning
control.

@margin-note{Xiden prohibits execution of any file that the user does not trust.}
@racketid[P] will search for an executable with the name bound to
@racket[command]. It starts by checking @racket[(file-exists?
command)]. If a file does not exist, then @racketid[P] will use
@racket[find-executable-path].

The result is @racket[FAILURE] if the @racket[command] is not found,
the subprocess takes longer than @racket[timeout] seconds, if the exit
code of the subprocess is not a member of @racket[expected-exit-codes]
(when @racket[expected-exit-codes] is not null), or if standard error
holds at least one byte when @racket[fail-on-stderr?]  is
@racket[#t]. Otherwise, the result is @racket[(void)].

The program log will gain either a @racket[$subprocess:report] if the
subprocess ran, or a @racket[$subprocess:command-not-found] if
@racket[command] was not found.

Standard input is drawn from @racket[stdin]. If @racket[stdin] is
@racket[#f], then no standard input will be available to the
subprocess. Otherwise, a new file stream port is opened on the
subprocess and all contents from @racket[stdin] are copied to the
subprocess before analyzing the subprocess' behavior.

Standard output and standard error are both forwarded to
@racket[(current-output-port)] and @racket[(current-error-port)],
respectively.

The @racket[controller] argument is private. It's used to mock the
effects of subprocesses on program flow for testing purposes.  The
default value tells @racket[run] not to mock any operations.
}


@defstruct*[($subprocess $message) ()]{
A @tech{message} pertaining to a subprocess.
}

@defstruct*[($subprocess:command-not-found $message) ([cmd string?])]{
A subprocess could not start because the @racket[cmd] was not found on
the system.
}

@defstruct*[($subprocess:report $message)
            ([cmd path-string?]
             [args (listof (or/c path-string? string-no-nuls? bytes-no-nuls?))]
             [max-runtime exact-positive-integer?]
             [actual-runtime (>=/c 0)]
             [expected-exit-codes (listof (integer-in 0 255))]
             [actual-exit-code (integer-in 0 255)]
             [stderr? boolean?])]{
A message about a subprocess started using @racket[cmd] and @racket[args].

Note that @racket[cmd] is bound to the path of the command that
actually ran, not the path passed to a call to @racket[run]. If
@racket[cmd] is a relative path, then the path is with respect to the
directory in which a package was executing.

@racket[max-runtime] is bound to the value passed as the
@racketid[timeout] argument in @racket[run]. @racket[actual-runtime]
is the number of seconds the subprocess took to finish, rounded down.

@racket[expected-exit-codes] is bound to the value passed to the
same argument in @racket[run]. @racket[actual-exit-code] is the
exit code returned from the subprocess.

If the subprocess places one byte on standard error, then
@racket[stderr?] is @racket[#t].
}
