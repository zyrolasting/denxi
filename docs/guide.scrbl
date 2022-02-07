#lang reader "../document.rkt"

@title{Denxi Guide}
@by-slg

This guide is for developers who read @other-doc[denxi-white-paper],
and have a working knowledge of Racket. By reading this guide, you
will write programs in Denxi in preparation for reading
@other-doc[denxi-reference].

For all documentation, see @other-doc[denxi-index].


@section{Launchers}

A @deftech{launcher} is a program that can distribute programs.  A
launcher used as an entry point to Denxi may define safety limits for
the process.

Since launchers are supersets of @racketmodname[racket/base], they can
use the rest of Racket to extend Denxi for specialized distribution
problems. Examples include @hyperlink[ex1]{defining a host's OpenSSL
instance as a cryptographic backend}, @hyperlink[ex2]{resolving
package conflicts locally}, @hyperlink[ex3]{managing multiple Racket
versions}, and @hyperlink[ex0]{forcing use of one set of generated
bindings}.

You may wish to skip these examples and start reading
@other-doc[denxi-reference], but you'll struggle to navigate the
reference without the examples or this guide to set context.

You may review the examples through the links above, but all of the
examples have been shipped to you when you installed Denxi. Run this
command to print the examples directory on your disk. Go to that
directory and look for a README. The README picks up immediately after
this guide, and shows you a suggested reading order.

@verbatim[#:indent 2]|{
racket -e '(~a (collection-file-path "examples" "denxi"))'
}|

With enough practice you will write your own catalogs and launchers to
distribute. If you manage to do so, then thank you for using Denxi!
