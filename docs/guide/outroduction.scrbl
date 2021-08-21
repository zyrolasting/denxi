#lang scribble/manual

@require["../shared.rkt"]

@title[#:tag "outroduction"]{Practice}

@(define ex0 "https://github.com/zyrolasting/xiden/tree/master/examples/generated-racket-bindings")
@(define ex1 "https://github.com/zyrolasting/xiden/tree/master/examples/cryptography-backends")
@(define ex2 "https://github.com/zyrolasting/xiden/tree/master/examples/output-conflicts")
@(define ex3 "https://github.com/zyrolasting/xiden/tree/master/examples/racket-installation-manager")

You installed Xiden, wrote a package definition, wrote a launcher,
then used the launcher to (un)install something.

This guide leaves out a lot of detail to make one point:
@italic{everything boils down to launchers and package
definitions}. Now that you know what both are, you can read built-in
examples and write your own. Doing so will help you understand
@other-doc[xiden-reference].

The toy examples we covered in this guide are for context only. The
built-in examples are more interesting. You will learn how to
@hyperlink[ex1]{define a host's OpenSSL instance as a cryptographic
backend}, @hyperlink[ex2]{resolve package conflicts},
@hyperlink[ex3]{manage multiple Racket versions}, and
@hyperlink[ex0]{prevent two lexically identical structs from
generating non-@racket[eq?] bindings}.

You may review the examples through the links above, but all of the
examples have been shipped to you in Xiden's source. Run this command
to print the examples directory on your disk. Go to that directory and
look for a README. The README picks up from immediately after this
guide, and shows you a suggested reading order.

@verbatim[#:indent 2]|{
racket -e '(~a (collection-file-path "examples" "xiden"))'
}|

With enough practice you will write a custom launcher that is useful
to others.  You can distribute that custom launcher using
@litchar{xiden}, which creates more confidence in both Xiden and your
launcher.

Thank you for trying Xiden, and I hope it brings you good fortune. My
name is Sage, and you can reach me through my
@hyperlink["https://sagegerard.com"]{website}.
