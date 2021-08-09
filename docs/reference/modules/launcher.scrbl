#lang scribble/manual

@require[@for-label[racket/base
                    xiden/launcher]
                    "../../shared.rkt"]

@title{Launchers}

@(defmodulelang* (xiden/launcher))

A @deftech{launcher} is a Racket module defined by a user for the
purposes of extending, configuring, then launching Xiden.  A launcher
starts with the same level of privilege as the OS-level user because
it executes in advance of Xiden's restrictions, so a programmer must
exercise caution with untrusted data.

The @racketmodname[xiden/launcher] language is a superset of
@racketmodname[racket/base] that reprovides all modules in the
@tt{xiden} collection except @racketmodname[xiden/pkgdef].
