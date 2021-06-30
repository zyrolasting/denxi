#lang scribble/manual

@require["../shared.rkt"]

@title[#:tag "guide-end"]{Now What?}

You installed Xiden, wrote a package definition, installed software
using your own launcher, then uninstalled that software.

We did @italic{not} discuss integrity checking, signature
verification, package conflicts, dependency hell, or other problems
that plague software distribution. Xiden handles them, which means
more learning.

I mentioned that your motiviation for learning comes from Xiden's
benefits, but there's one other detail you might appreciate now: You
can distribute your custom launcher using @litchar{xiden}. That is,
Xiden can distribute its own alternative interfaces.

To continue, run this command to print where Xiden's examples are on
your disk. Go to that directory and look for a README to continue.

@verbatim[#:indent 2]|{
racket -e '(~a (collection-file-path "examples" "xiden"))'
}|

Thank you for reading this guide, and for trying Xiden. My name is
Sage, and you can email me at the address found on
@hyperlink["https://sagegerard.com"]{my page}.
