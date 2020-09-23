#lang scribble/manual

@(require (for-label racket/base racket/contract xiden/rc)
          xiden/rc
          xiden/cli-flag
          "../shared.rkt")

@title{Runtime Configuration}

@defmodule[xiden/rc]

@racketmodname[xiden/rc] provides several @tech/reference{parameters}
that change how @project-name behaves to reflect the user's
wishes. This section documents each parameter with its command-line
flags, contract, and default value.

By convention, every parameter's identifier can be reused in an
environment variable, the @litchar{etc/xiden.rkt} file in a
@tech{workspace}, or as a long command line flag.

Given a parameter's identifier @racket[P], the environment variable
@racket[P] overrides @racket[P]'s hard-coded default when set to a
non-empty string.  A value bound to @racket[P] in the runtime
configuration file overrides the environment variable. Finally, a
value passed to a command line flag defined for @racket[P] overrides
the value from the runtime configuration file.

@(require (for-syntax racket xiden/rc xiden/setting))

@(define-for-syntax (get-contract-datum s)
  (define proc (setting-valid? s))
  (define formatted (~v proc))
  (if (string-prefix? formatted "#<")
      (object-name proc)
      (read (open-input-string formatted))))

@(define-for-syntax (setting->doc s)
  `(defthing #:kind "setting"
             ,(setting-id s)
             (parameter/c ,(get-contract-datum s))
             #:value ,(s)
             ,(setting-description s)))

@(define-for-syntax (loud v)
  (writeln v) v)

@(define-syntax (defsetting* stx)
  (datum->syntax stx `(begin . ,(map setting->doc (hash-values XIDEN_SETTINGS))) stx))

@defsetting*[]
