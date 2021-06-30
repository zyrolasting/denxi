#lang scribble/manual

@require["../shared.rkt"
         @for-label[racket/base
                    xiden/cli
                    xiden/integrity]]

@title[#:tag "launchers"]{Launchers}

This @deftech{launcher} program starts and controls Xiden.

@racketmod[#:file "my-xiden.rkt"
xiden/launcher
(module+ main (launch-xiden!))
]

Xiden built-in launcher is called @litchar{xiden}, and it actually has
the same implementation! The difference is that we control our
launchers. Save the above code into @litchar{my-xiden.rkt} and give it
the definition from the previous section.

@verbatim[#:indent 2]|{
$ racket my-xiden.rkt do ++install-abbreviated definition.rkt
}|

This command will give you first of many reasons for why it won't
work, because all launchers start with @deftech{zero-trust}. You must
explicitly allow @italic{all relevant details} for security. We won't
neglect security, but we'll use this production-unsafe launcher until
you can work with zero-trust.

@racketmod[#:file "my-xiden.rkt"
xiden/launcher

(current-chfs (list snake-oil-chf))
(XIDEN_TRUST_BAD_DIGEST #t)
(module+ main (launch-xiden!))
]

This launcher has a low bar, so run the same @litchar{do} command. A
symbolic link will appear. Compare the definition to output to start
connecting some dots.  To uninstall the output, delete the link and
run the garbage collection command.

@verbatim[#:indent 2]|{
$ rm my-first-package && racket my-xiden.rkt gc
Recovered 13 bytes
}|

To recap, launchers build software from package definitions and issue
links to their outputs. You're now equipped to learn more by example.
