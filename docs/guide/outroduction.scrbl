#lang scribble/manual

@require["../shared.rkt"]

@title[#:tag "outroduction"]{Practice}

You installed Xiden, wrote a package definition, wrote a launcher,
then used the launcher to (un)install something.

This guide left a lot out to make one point: @italic{everything in
Xiden boils down to launchers and package definitions}. Now that you
know what both are, you can understand examples in context. Run this
command to print where Xiden's examples are on your disk. Go to that
directory and look for a README. From there, practice.

@verbatim[#:indent 2]|{
racket -e '(~a (collection-file-path "examples" "xiden"))'
}|

At the end you'll be comfortable reading @other-doc[xiden-reference]
and can organize your own software distribution setup. Once you
distribute your custom launcher using @litchar{xiden}, things really
start to open up.

Thank you for trying Xiden, and I hope it brings you good fortune. My
name is Sage, and you can reach me through my
@hyperlink["https://sagegerard.com"]{website}.
