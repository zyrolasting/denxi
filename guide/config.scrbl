#lang scribble/manual

@require["shared.rkt" @for-label[racket/base]]

@title{Configuration}

Let a hypothetical setting be “Use Widget”, which is a boolean.  The canonical
identifer for of the setting is @racket[ZCPKG_USE_WIDGET], meaning we
capitalize the letters, replace spaces with underscores, and prefix the result
with @racket[ZCPKG_].

The value of @racket[ZCPKG_USE_WIDGET] is the last defined item in this list:

@itemlist[#:style 'ordered

@item{A hard-coded default.}

@item{A value bound to @tt{ZCPKG_USE_WIDGET} in a @tech{workspace}'s @tt{etc/zcpkg.rktd}.}

@item{An environment variable named @tt{ZCPKG_USE_WIDGET}, which must be set to
a string containing a @racket[read]-able value
(e.g. @tt{ZCPKG_USE_WIDGET="#t"}).}

@item{A command-line flag named @litchar{--use-widget}. Since our example is a
boolean, the flag itself will suffice. Otherwise a string containing a
@racket[read]-able value must follow the flag.}

]

Using Racket literals in strings enables consistent behavior across
configuration sources, but it has a tradeoff: @litchar{--flag "foo"} means
@racket['foo] in the program, and @litchar{--flag '"foo"'} means
@racket["foo"]. @binary will account for these differences when it makes sense,
but be sure to think in terms of stringified Racket values when configuring it
from a command line or environment variables.

Use the @litchar{zcpkg config} command to manage @|binary|'s configuration. I'll
use the @tt{ZCPKG_VERBOSE} setting for the below examples:

@itemlist[
@item{@litchar{zcpkg config dump}: Prints a readable hash of the active configuration.}
@item{@litchar{zcpkg config set ZCPKG_VERBOSE "t"}: Changes a setting in a @tech{workspace}'s @tt{etc/zcpkg.rkt} file.}
@item{@litchar{zcpkg config get ZCPKG_VERBOSE}: Gets the value of a setting}
]
