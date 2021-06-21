#lang scribble/manual

@require[@for-label[racket/base
                    xiden/launcher]
                    "../../shared.rkt"]

@title{Launchers}

@(defmodulelang* (xiden/launcher))

A @deftech{launcher} is a Racket module defined by a user for the
purposes of launching Xiden with specific options.  A launcher uses
the same level of privilege as the OS-level user because it executes
in advance of Xiden's restrictions.

@racketmodname[xiden/launcher] is a superset of
@racketmodname[racket/base] that reprovides
@racketmodname[xiden/archive], @racketmodname[xiden/artifact],
@racketmodname[xiden/cli], @racketmodname[xiden/codec],
@racketmodname[xiden/state], @racketmodname[xiden/integrity],
@racketmodname[xiden/crypto], @racketmodname[xiden/format],
@racketmodname[xiden/package], @racketmodname[xiden/printer],
@racketmodname[xiden/security], @racketmodname[xiden/signature],
@racketmodname[xiden/source], @racketmodname[xiden/state],
@racketmodname[xiden/subprogram], and @racketmodname[xiden/system].
