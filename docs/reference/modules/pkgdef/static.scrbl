#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/path
                    racket/pretty
                    racket/string
                    syntax/modread
                    denxi/codec
                    denxi/integrity
                    denxi/crypto
                    denxi/pkgdef/static
                    denxi/racket-module
                    denxi/string]
         "../../../shared.rkt"]

@title{Static Operations for Package Definitions}

@defmodule[denxi/pkgdef/static]


@deftogether[(
@defthing[PACKAGE_DEFINITION_MODULE_LANG symbol?]
@defthing[PACKAGE_DEFINITION_READER_LANG symbol?]
)]{
Collection paths for a module language and reader extension used to
write package definitions.
}

@defthing[current-package-definition-editor
          (parameter/c (-> bare-pkgdef? (subprogram/c bare-pkgdef?)))]{
A parameter for a procedure that overrides @tech{package definition}s
before use. Defaults to @racket[subprogram-unit].
}

@defproc[(load-package-definition [source source?]
				  [max-size budget/c]
                                  [editor
				   (-> bare-pkgdef? (subprogram/c bare-pkgdef?))
                                   (current-package-definition-editor)])
				  (subprogram/c bare-pkgdef?)]{
Returns a @tech{subprogram} that loads a package definition into
memory from a @tech{source}. The package definition's actual content
depends on @racket[editor].
}


@defproc[(make-package-definition-datum [#:id id symbol? 'pkgdef] [body list?]) package-definition-datum?]{
Equivalent to @racket[(make-racket-module-datum #:id id PACKAGE_DEFINITION_MODULE_LANG body)]
}


@defproc[(get-package-definition-body [datum package-definition-datum?]) list?]{
Returns the top-level forms of the module code in @racket[datum].
}


@defthing[bare-pkgdef? flat-contract?
          #:value (struct/c bare-racket-module symbol?
                            PACKAGE_DEFINITION_MODULE_LANG
                            list?)]{
A contract that matches a @tech{bare} @tech{package definition}.
}


@defthing[package-definition-datum? predicate/c]{
Equivalent to @racket[(racket-module-code? PACKAGE_DEFINITION_MODULE_LANG v)].
}


@defproc[(get-static-simple-string [pkgdef bare-pkgdef?] [id symbol?]) any/c]{
Equivalent to @racket[(get-static-simple-value stripped id "default")].

The return value is assumed to be a string, but might not be.
}


@defproc[(get-static-inputs [pkgdef bare-pkgdef?]) list?]{
Returns a list of all input expressions in @racket[pkgdef].
}


@defproc[(get-static-simple-value [pkgdef bare-pkgdef?] [id symbol?] [default any/c]) any/c]{
Searches the top-level code of @racket[pkgdef] for a S-expression of
form @racket[(id val)]. Returns the datum in @racket[val]'s position,
or @racket[default] if no such expression exists.
}


@defproc[(get-static-list-value [pkgdef bare-pkgdef?] [id symbol?] [default any/c]) any/c]{
Searches the top-level code of @racket[pkgdef] for a S-expression of
form @racket[(id . xs)]. Returns @racket[xs], or @racket[default] if
no such expression exists.
}


@defproc[(override-inputs [pkgdef bare-pkgdef?] [input-exprs list?]) bare-pkgdef?]{
Functionally replaces any @tech{package inputs} in @racket[pkgdef]
that share a name with at least one element in @racket[input-exprs].

If multiple expressions in @racket[input-exprs] share a name, only the
last occurrance will be used in the output.
}


@defproc[(replace-input-expression [pkgdef bare-pkgdef?]
                                   [input-name non-empty-string?]
                                   [replacement any/c])
                                   bare-pkgdef?]{
Functionally replaces the first @racket[input] form
in @racket[pkgdef] with @racket[replacement], where the input's name
is @racket[input-name].
}
