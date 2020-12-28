#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/format
                    racket/path
                    racket/pretty
                    syntax/modread
                    xiden/logged
                    xiden/racket-module]
         "../../shared.rkt"]

@title[#:tag "rktmod"]{Static Operations for Racket Module Syntax and Expressions}

@defmodule[xiden/racket-module]

@defthing[racket-module-input-variant/c flat-contract? #:value (or/c path? list? string? bytes? input-port?)]{
A contract matching accepted value types when reading a Racket module.
}

@defthing[racket-module-variant/c flat-contract? #:value (or/c syntax? list?)]{
A contract matching accepted value types for an in-memory representation of Racket module code.
}

@defproc[(racket-module-code? [modlang symbol?] [v racket-module-variant/c]) boolean?]{
Returns @racket[#t] if @racket[(get-racket-module-body modlang v)]
would not fail. This does not verify the functionality of the module.
}


@defproc[(get-racket-module-body [expected-lang (or/c #f symbol?)] [code racket-module-variant/c]) (or/c #f list? syntax?)]{
Returns the top level forms of @racket[code], or @racket[#f] if the
module language does not match @racket[expected-lang]. If
@racket[expected-lang] is @racket[#f], then it is not checked.

The return type value mirrors that of the input value type.  That is,
@racket[(list? code)] implies @racket[(list? (get-racket-module-body
#f code))].
}

@defproc[(make-racket-module-datum [#:id id symbol? 'anon] [lang symbol?] [top-level list?]) list?]{
Equivalent to @racket[(apply list 'module id lang top-level)].
}


@defproc[(read-racket-module [expected-reader-lang symbol?]
                             [expected-module-lang symbol?]
                             [variant racket-module-input-variant/c])
                             (logged/c syntax?)]{
Returns a @tech{logged procedure} that reads a Racket module's code
and returns the resulting syntax object with minimal
expansion. Reading is limited to the reader extension provided by the
collection path in @racket[expected-reader-lang], and the module
language must match @racket[expected-module-lang].

If @racket[variant] is an input port, @racket[read-racket-module]
will attempt to read code directly from @racket[variant].

If @racket[variant] is a path, string, or byte string, then the
behavior is equivalent to applying @racket[read-racket-module] to an
input port for the file, string content, or byte content respectively.

If @racket[variant] is a list, then the behavior is equivalent to
applying @racket[read-racket-module] to an input port for @racket[(~s
variant)].
}

