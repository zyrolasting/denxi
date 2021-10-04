#lang scribble/manual

@require[@for-label[racket/base
                    denxi/launcher]
                    "../../shared.rkt"]

@title{Launchers}

@(defmodulelang* (denxi/launcher))

A @deftech{launcher} is a Racket module defined by a user for the
purposes of extending, configuring, then launching Denxi.  A launcher
starts with the same level of privilege as the OS-level user because
it executes in advance of Denxi's restrictions, so a programmer must
exercise caution with untrusted data.

The @racketmodname[denxi/launcher] language is a superset of
@racketmodname[racket/base] that reprovides all modules in the
@tt{denxi} collection except @racketmodname[denxi/pkgdef].
