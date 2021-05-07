#lang scribble/manual

@require[@for-label[xiden/package]
         "../shared.rkt"]

@title{Overriding Inputs}

You may override @tech/xiden-guide{package inputs} using some launcher
@litchar{xi}. One way is to use the command line interface.

@verbatim|{
$ xi do +a definition.rkt +o '^leagues:baseball' '(input "umpire" ...)'
}|

The command line adds overrides to @racket[XIDEN_INPUT_OVERRIDES],
which applies to all packages in the scope of a transaction.  Each
override takes two arguments. The first is a readable Perl-style
(@racket[pregexp]) regular expression that matches against a
@tech/xiden-guide{package query}. The second is the code for an input
expression.

When Xiden processes packages, it will build a package query using
only the provider, package name, edition and revision number
(e.g. @racket{leagues:baseball:pro:89}). If the pattern matches,
then the input expression provided in the command replaces the
input expression of the same name in that package.

The override applies to all eligible packages, and all inputs of the
same name. This allows you to standardize dependencies in the event
you end up with something like multiple slightly different copies of
Ruby.

@verbatim|{
$ RUBY='(input "ruby" ...)'
$ xi do +a definition.rkt \
        +o 'syntax-highlighting' "$RUBY" \
        +o 'images' "$RUBY"
}|

