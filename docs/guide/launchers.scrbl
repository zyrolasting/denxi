#lang scribble/manual

@require["../shared.rkt"
         @for-label[racket/base
                    xiden/cli
                    xiden/codec
                    xiden/integrity
                    xiden/signature]]

@title[#:tag "launchers"]{Launchers}

A @deftech{launcher} is an app that starts and uses Xiden. We launch
Xiden's built-in launcher with the @litchar{xiden} command. Run it now
with no arguments, and remember what the output looks like.

Xiden observes @deftech{zero-trust architecture} (“@deftech{ZTA}”)
principles. It refuses to do anything you did not explicitly allow.
@litchar{xiden} is an equally bureaucratic @deftech{zero-trust
launcher}.

We'll make things easier by writing a custom launcher. Save this code
to a new @litchar{my-xiden.rkt} launcher file.

@racketmod[#:file "my-xiden.rkt"
xiden/launcher

(module+ main (launch-xiden!))
]

The output of @litchar{racket my-xiden.rkt} is the same as just
@litchar{xiden}. They make no change to default settings, so they are
both the same @tech{zero-trust launcher} implementation. The
difference is that in our launcher, we can control Xiden with trusted
code.

To install software, we'll give our @litchar{definition.rkt} file from
the last section to the @litchar{do} command. @litchar{do} runs a
transaction against your file system. The word “transaction”
essentially means “Install what I give you. If there's any problem,
put everything back the way it was.”

@verbatim[#:indent 2]|{
$ racket my-xiden.rkt do ++install-abbreviated definition.rkt
}|

It's not going to work, and it doesn't even matter what output you see
at this point. The command is just going to show you the first of many
messages that explain why it @italic{didn't} install the software,
because of @tech{ZTA}.

From here we could discuss the command line arguments to finish the
installation. Or, we could be bad and disable data verification with
our launcher. Don't use this launcher in security-critical code!

@racketmod[#:file "my-xiden.rkt"
xiden/launcher

(current-chfs (list snake-oil-chf))
(XIDEN_TRUST_BAD_DIGEST #t)

(module+ main (launch-xiden!))
]

We won't neglect security, we just don't address it here for brevity.
After the guide, you'll be shown examples that build on top of what
you'll learn here.

Run the installation one last time, and you'll see a symbolic link
appear that points to installed software. The installed software is
actually a composition of symbolic links that point to data resolved
during the transaction. This is a powerful setup because it allows
Xiden to prevent duplication on a per-input level.

@verbatim[#:indent 2]|{
.
├── definition.rkt
├── my-first-package -> $HOME/.xiden/objects/mrtkmty0b3txrhe3gn25s7tvg3wkf63c
│   └── hello.txt -> ../jgx70b86yd2skbq1z3d8xyfq55g33nms
└── my-xiden.rkt
}|

To uninstall the software, delete the link and collect garbage using
the @litchar{gc} command. Xiden will delete the files that have no
incoming links.

@verbatim[#:indent 2]|{
$ rm my-first-package
$ racket my-xiden.rkt gc
Recovered 13 bytes
}|

Notice that we didn't change the install command.  Our launcher
configured Xiden such that the default CLI no longer required flags to
complete our transaction.
