#lang scribble/manual

@require["../shared.rkt"
         @for-label[racket/base
                    denxi/cli
                    denxi/integrity]]

@title[#:tag "launchers"]{Launchers}

This @deftech{launcher} program starts and controls Denxi.

@racketmod[#:file "my-denxi.rkt"
denxi/launcher
(module+ main (launch-denxi!))
]

Denxi built-in launcher is called @litchar{denxi}, and it actually has
the same implementation! The difference is that we control our
launchers. Save the above code into @litchar{my-denxi.rkt} and give it
the definition from the previous section.

@verbatim[#:indent 2]|{
$ racket my-denxi.rkt do ++install-abbreviated definition.rkt
}|

This command will give you first of many reasons for why it won't
work, because all launchers start with @deftech{zero-trust}. You must
explicitly allow @italic{all relevant details} for security. We won't
neglect security, but we'll use this production-unsafe launcher until
you can work with zero-trust.

@racketmod[#:file "my-denxi.rkt"
denxi/launcher

(current-chfs (list snake-oil-chf))
(DENXI_TRUST_BAD_DIGEST #t)
(module+ main (launch-denxi!))
]

This launcher has a low bar, so run the same @litchar{do} command. A
symbolic link will appear. Compare the definition to output to start
connecting some dots.  To uninstall the output, delete the link and
run the garbage collection command.

@verbatim[#:indent 2]|{
$ rm my-first-package && racket my-denxi.rkt gc
Recovered 13 bytes
}|

To recap, launchers build software from package definitions and issue
links to their outputs.

To be fair, this is a toy example, and toy examples are illustrative
at best.  The good news is that you are now equipped to review working
examples. As the examples become more advanced, they incorporate more
real-world information.
