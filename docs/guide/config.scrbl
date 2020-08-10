#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Configuring @|binary|}

You can configure @|binary| using @racket[(read)]able Racket literals.
This takes some getting used to when interacting with @binary from a
shell, because @litchar{--flag "foo"} means @racket['foo] during
runtime, and @litchar{--flag '"foo"'} means @racket["foo"]. @binary
will account for these differences so long as it does not result in
surprising behavior. It also provides a REPL to help you work outside
of shell conventions.

Why do it this way? Because it makes the configuration experience
consistent, and requires little added knowledge. Using literals also
means the exact Racket value you type is used in @|binary|, under
the protection of relevant contracts.


@section{Settings}

A @deftech{setting} is an object that returns a Racket value
from at least one source for configuration purposes.

The sources used by a setting are derived from an English name for the
setting. The @deftech{canonical setting name} is the uppercase variant of the
English name prefixed by @racket[ZCPKG_], with non-alphanumeric
characters replaced by underscores.

For example, the setting “Use Widget” has the @tech{canonical setting name}
@racketfont{ZCPKG_USE_WIDGET}.

@section[#:tag "multi-source"]{Multi-Source Settings}

Most @tech{settings} use multiple sources. The value of the
hypothetical multi-source @racket[ZCPKG_USE_WIDGET] setting is the
last defined item in this list:

@itemlist[#:style 'ordered

@item{A default value hard-coded in @|binary|.}

@item{A value bound to @tt{ZCPKG_USE_WIDGET} in a @tech{workspace}'s @tt{etc/zcpkg.rkt}.}

@item{An environment variable named @tt{ZCPKG_USE_WIDGET}, which must be set to
a string containing a @racket[read]-able value
(e.g. @tt{ZCPKG_USE_WIDGET="#t"}).}

@item{A command-line flag named @litchar{--use-widget}. Since our example is a
boolean, the flag itself will suffice. Otherwise a string containing a
@racket[read]-able value must follow the flag.}

@item{A programmatically-set override value.}

]


@section{Setting the Workspace}

@racket[ZCPKG_WORKSPACE] defines the directory to use as the
@tech{workspace}.

@racket[ZCPKG_WORKSPACE] can only be set using the environment
variable of the same name. This is because all other settings check a
@tech{workspace}-specific configuration file, and that file cannot be
known until @racket[ZCPKG_WORKSPACE] is set.

@racket[ZCPKG_WORKSPACE] must be a @racket[complete-path?]. If the
path points to an existing entry on the filesystem, then that entry
must be a directory. If the path points to no existing entry, then
@binary will create a directory at that path.


@section{The @tt{config} command}

Use the @litchar{zcpkg config} command to manage @|binary|'s configuration.

I'll use the @tt{ZCPKG_VERBOSE} setting for the below examples:

@itemlist[
@item{@litchar{zcpkg config dump}: Prints a readable hash of the active configuration.}
@item{@litchar{zcpkg config set ZCPKG_VERBOSE "#t"}: Changes a setting in a @tech{workspace}'s @tt{etc/zcpkg.rkt} file.}
@item{@litchar{zcpkg config get ZCPKG_VERBOSE}: Gets the value of a setting}
]

@section{Using the Configuration REPL}

If the shell conventions of your platform make configuration too
painful, then use @litchar{zcpkg config repl}. This will drop you into
a limited REPL with bindings to all settings except @racket[ZCPKG_WORKSPACE]
and a procedure called @racket[save!]. @racket[save!] will save all of
the current settings to the runtime configuration file in the
@tech{workspace}.

Every @tech{setting} is bound to an identifier in the form of a
@tech{canonical setting name}. The setting is a procedure.  If you
apply this procedure to no arguments, you will get the current value
of the setting as @|binary| sees it during runtime.

@racketinput[(ZCPKG_VERBOSE)]
@racketresult[#f]

If you apply the setting to one argument, then you will override that
setting for the session. Don't worry about making mistakes. Every
setting is guarded by a @tech/reference{contract}.

@racketinput[(ZCPKG_VERBOSE 'yes)]
@racketresult{expected: (or/c void? boolean?)}
@racketinput[(ZCPKG_VERBOSE)]
@racketresult[#f]
@racketinput[(ZCPKG_VERBOSE #t)]
@racketinput[(ZCPKG_VERBOSE)]
@racketresult[#t]

Notice that each setting accepts @racket[(void)]. This has the effect
of clearing your override. When you request the current value of the
setting, that value will be computed according to @secref{multi-source}.

@racketinput[(ZCPKG_VERBOSE (void))]
@racketinput[(ZCPKG_VERBOSE)]
@racketresult[#f]

To review the values of all settings, call @racket[dump].
You'll see a hash of the complete current configuration.

@racketinput[(dump)]

When you are satisfied with your changes, call @racket[save!].  This
will write all current settings to your @tech{workspace}'s
configuration file.

@racketinput[(save!)]
