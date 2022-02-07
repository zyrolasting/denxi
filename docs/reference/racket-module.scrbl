#lang denxi/document

@title[#:tag "rktmod"]{Racket Module Operations}

@defmodule[denxi/racket-module]


@defthing[racket-module-input-variant/c flat-contract? #:value (or/c path? list? string? bytes? input-port?)]{
A contract matching accepted value types when reading a Racket module.
}

@defproc[(list/syntax? [v any/c]) boolean?]{
Returns @racket[(and (syntax? v) (list? (syntax-e v)))].
}

@defthing[code/c flat-contract? #:value (or/c list/syntax? list?)]{
A contract matching accepted value types for an in-memory
representation of a Racket program. In the context of this library,
this can either be a Racket module form, or some list of instructions
and/or data.
}

@defthing[racket-module-variant/c flat-contract? #:value (and/c code/c any-racket-module-code?)]{
A contract matching accepted value types for an in-memory representation of Racket module code.
}

@defproc[(any-racket-module-code? [v any/c]) boolean?]{
Returns @racket[(racket-module-code? #f v)].
}

@defproc[(racket-module-code? [modlang symbol?] [v any/c]) boolean?]{
Returns @racket[#t] if @racket[(get-racket-module-body modlang v)]
would not fail. This does not verify the functionality of the module.
}

@defproc[(get-racket-module-body [expected-lang (or/c #f symbol?)] [code racket-module-variant/c]) (or/c #f code/c)]{
Returns the top level forms of @racket[code], or @racket[#f] if the
module language does not match @racket[expected-lang]. If
@racket[expected-lang] is @racket[#f], then the module language is not
checked.

The return type value mirrors that of the input value type.  That is,
@racket[(list? code)] implies @racket[(list? (get-racket-module-body
#f code))].
}

@defproc[(make-racket-module-datum [#:id id symbol? 'anon] [lang symbol?] [top-level list?]) list?]{
Equivalent to @racket[(apply list 'module id lang top-level)].
}


@defproc[(coerce-datum [code code/c]) list?]{
If @racket[code] is a list, then returns @racket[code].
Otherwise, returns @racket[(syntax->datum code)].
}

@defproc[(read-racket-module [expected-reader-lang symbol?]
                             [expected-module-lang symbol?]
                             [variant racket-module-input-variant/c])
                             (subprogram/c syntax?)]{
Returns a @tech{subprogram} that reads a Racket module's code
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


@section{Stripping and Dressing}

To @deftech{strip} a Racket module in @racket[code/c] form is to
convert it to a @tech{bare} module. To @deftech{dress} the module is
to do the reverse. Stripping and dressing are not inverse operations,
because stripping does not preserve lexical and location information.

In the context of Denxi, programs are restricted to a small, fixed
grammar and are subject to meaningful module-level overrides. This
makes source location information meaningless in some cases, but
lexical information remains constant. Denxi uses @tech{bare modules}
when only the content matters for code transformations.

@defstruct*[bare-racket-module ([name symbol?] [lang symbol?] [code list?]) #:transparent]{
A @deftech{bare} Racket module (or “@deftech{bare module}”) is an
instance of @racket[bare-racket-module].

Each instance holds parts of a Racket module code without lexical
information, source location information, or an implicit form for a
module body such as @racket[#%plain-module-begin].
}


@defproc[(strip [module-datum code/c]) bare-racket-module?]{
@tech{Strips} a Racket module.

@racketinput[(equal? (strip #'(module anon racket/base (#%module-begin (define a 1) (provide a))))
                     (bare-racket-module 'anon 'racket/base '((define a 1) (provide a))))]
}

@defproc[(dress [stripped bare-racket-module?]) list?]{
Returns

@racketblock[
(make-racket-module-datum
  #:id
  (bare-racket-module-name stripped)
  (bare-racket-module-lang stripped)
  (bare-racket-module-code stripped))]
}

@section{Fetching Racket Modules in Package Definition}

@defproc[(keep-standalone-racket-module [#:compile-with compile-with (or/c #f path-string?) "raco"]
                                        [name string?])
                                        subprogram?]{
Behaves like @racket[keep-input] when @racket[compile-with] is
@racket[#f].

If @racket[compile-with] is not @racket[#f], it is used as the first
argument to @racket[run] for selecting a @tt{raco} executable.  Once
the module is available, it is immediately compiled using the
@tt{make} subcommand.
}
