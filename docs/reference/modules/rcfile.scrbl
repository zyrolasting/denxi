#lang scribble/manual

@(require (for-label (except-in xiden/rcfile #%module-begin)
                     xiden/rc
                     xiden/setting)
          "../../shared.rkt")

@title{Runtime Configuration Files}

@(defmodulelang* (xiden/rcfile))

A @deftech{runtime configuration file}, or @deftech{rcfile}, is a
@racketmodname[xiden/rcfile] Racket module.  It defines values for a
@tech{runtime configuration} using identifiers that match the
@racket[setting-id] of each defined
@tech{setting}. e.g. @racket[(define XIDEN_VERBOSE #t)].

The @deftech{target runtime configuration file}, or @deftech{target
rcfile}, is located at @litchar{etc/xiden.rkt} with respect to the
@tech{target workspace}.

The @racketmodname[xiden/rcfile] collection path can be used as a
module language or as a reader extension.

The grammar is a superset of @racketmodname[setup/infotab], or
the @racketmodname[info] language. It includes the following
bindings:

@(define-syntax-rule (I id)
  (item (racket id)))

@itemlist[
@I[integrity]
@I[base32]
@I[base64]
@I[hex]
]
