#lang scribble/manual

@(require (for-label racket/base racket/contract xiden/rc)
          xiden/rc
          xiden/cli-flag
          "../shared.rkt")

@title{Runtime Configuration}

@defmodule[xiden/rc]

@racketmodname[xiden/rc] provides several settings, which contain
@tech/reference{parameters} that change how @project-name behaves to reflect
the user's wishes. This section documents each setting with its command-line
flags, contract, and default value.

@section{Changing a Setting Value}

Here are the ways one can change a setting. Each method overrides
the method before it.

@itemlist[

@item{Do nothing. Every setting has a hard-coded default.}
@item{Set an environment variable, e.g. @litchar{export XIDEN_VERBOSE="#t"}.}
@item{Open @litchar{etc/xiden.rkt} in a @tech{workspace} and add @racket[(define XIDEN_VERBOSE #t)].}
@item{When applicable, use @litchar{--XIDEN_VERBOSE '#t'} in a command line (or an alternative flag).}
@item{In a program, use @racket[(parameterize ([(setting-derived-parameter XIDEN_VERBOSE) #t]) ...)].}

]


@section{Setting Reference}

These are the defined settings for @|project-name|, along with their default
values and command-line flags. Note that these entries are generated, and
only show the user-facing English string to describe the setting in the
context of a command. That context is not available here, so some entries
will be confusing until fixed.

@(require (for-syntax "../shared.rkt" racket xiden/rc xiden/setting xiden/cli-flag))

@(define-for-syntax (get-contract-datum s)
   (define proc (setting-valid? s))
   (define formatted (~v proc))
   (if (string-prefix? formatted "#<")
       (object-name proc)
       (read (open-input-string formatted))))


@(define-for-syntax (setting->doc s)
  (define cf (findf (λ (f) (eq? (cli-flag-setting f) s)) all-flags))
  `(defthing #:kind "setting"
             ,(setting-id s)
             ,(get-contract-datum s)
             #:value ,(s)
             (para ,(setting-description s))
             . ,(if cf
                    `("\nCLI Flags: " (litchar ,(format-cli-flags cf)))
                    null)))


@(define-syntax (defsetting* stx)
  (define sorted (sort #:key (lambda (p) (~a (car p))) (hash->list XIDEN_SETTINGS) string<?))
  (reformat-syntax stx `(begin . ,(map setting->doc (map cdr sorted)))))

@defsetting*[]
