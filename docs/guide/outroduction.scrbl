#lang scribble/manual

@require["../shared.rkt"]

@title[#:tag "outroduction"]{Next}

You installed Xiden, wrote a package definition, wrote a launcher,
then used the launcher to install and uninstall a file using the
definition.

We did @italic{not} discuss a lot of information, but this guide isn't
meant to front-load everything. It's only here to show you that
@italic{everything in Xiden boils down to launchers and package
definitions}. Now that you know what they are, you can understand
examples in context. Run this command to print where Xiden's examples
are on your disk. Go to that directory and look for a README. From
there, you'll learn by doing, and will start with baby steps.

@verbatim[#:indent 2]|{
racket -e '(~a (collection-file-path "examples" "xiden"))'
}|

At the end you'll be comfortable reading @other-doc[xiden-reference]
and can organize your own software distribution setup. You learn to
appreciate that @litchar{xiden} can distribute your launchers, and
will gain the benefits mentioned at the beginning of the guide.

Thank you for trying Xiden, and I hope it brings you good fortune. My
name is Sage, and you can reach me through my
@hyperlink["https://sagegerard.com"]{website}.
