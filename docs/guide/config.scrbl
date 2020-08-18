#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title[#:tag "config"]{Configuring @|binary|}

You can configure @|binary| using environment variables, the command
line interface, and/or a runtime configuration file. Every setting
has only one name, and a contract for accepted Racket values.

Configuration will feel awkward in your shell because @binary
emphasizes consistency to an almost religuous degree.

Let's look at the @racketfont{XIDEN_VERBOSE} setting, which is a
boolean. This list covers the possible sources of a value, where
each source overrides the one before it.

@itemlist[#:style 'ordered

@item{A default value hard-coded in @|binary|.}

@item{A value bound to @tt{XIDEN_VERBOSE} in a @tech{workspace}'s @tt{etc/xiden.rkt}.}

@item{A value set in the environment variable named @tt{XIDEN_VERBOSE}.
The value must be @racket[(read)]able (e.g. @tt{XIDEN_VERBOSE="#t"}).}

@item{A value set in the command-line flag named @litchar{--XIDEN_VERBOSE}.
The value must be @racket[(read)]able (e.g. @tt{--XIDEN_VERBOSE "#t"}).}

]


@section{AAAAH, how dare you! This makes command lines painful to type!}

Not always, but there is a tradeoff here. Since values are
@racket[read] from command line arguments and environment variables,
@litchar{--flag "foo"} means @racket['foo] in the program, and
@litchar{--flag '"foo"'} means @racket["foo"]. @binary will
account for these differences when it makes sense, and contracts
protect each setting. Don't worry too much about making a mistake
or typing more than you need to.

@secref{config-reply} covers a configuration REPL to help you work
outside of shell conventions when they become unwieldy.

@binary defines short flags, but values still need to be specified
(e.g. @litchar{-v "#t"}).

So why do it this way? What are the benefits of this approach?

@itemlist[

@item{It lowers the learning curve a bit. There's only one name for
every setting, and there's no translation from values like
@racket{true} to @racket[#t]. What you type is what gets used at
runtime, so long as it meets a contract.}

@item{
Let's say @racket[XIDEN_VERBOSE] defaults to @racket[#t]. You would not
be able to change that using a command-line argument without expressing
@racket[#f].
}

]


@section{Setting the Workspace}

@racket[XIDEN_WORKSPACE] defines the directory to use as the @tech{workspace}.

@racket[XIDEN_WORKSPACE] can only be set using the environment
variable of the same name. This is because all other settings check a
@tech{workspace}-specific configuration file, and that file cannot be
known until @racket[XIDEN_WORKSPACE] is set.

@racket[XIDEN_WORKSPACE] must be a @racket[complete-path?]. If the
path points to an existing entry on the filesystem, then that entry
must be a directory. If the path points to no existing entry, then
@binary will create a directory at that path.


@section{The @tt{config} command}

Use the @litchar{xiden config} command to manage @|binary|'s configuration.

I'll use the @tt{XIDEN_VERBOSE} setting for the below examples:

@itemlist[
@item{@litchar{xiden config dump}: Prints a readable hash of the active configuration.}
@item{@litchar{xiden config set XIDEN_VERBOSE "#t"}: Changes a setting in a @tech{workspace}'s @tt{etc/xiden.rkt} file.}
@item{@litchar{xiden config get XIDEN_VERBOSE}: Gets the value of a setting}
]

@section{Using the Configuration REPL}

If the shell conventions of your platform make configuration too
painful, then use @litchar{xiden config repl}. This will drop you into
a limited REPL with bindings to all settings and two procedures called
@racket[save!] and @racket[dump].

@racket[dump] will show you the entire configuration you've set.
@racket[save!] will save that same configuration to the runtime
configuration file in your @tech{workspace}.

Each setting identifier is bound to a procedure.  If you
apply this procedure to no arguments, you will get the current value
of the setting as @|binary| sees it during runtime.

@racketinput[(XIDEN_VERBOSE)]
@racketresult[#f]

If you apply the setting to one argument, then you define a new value
for that setting. The value only applies for the session, and will
not persist unless you @racket[save!] your work.

Don't worry about making mistakes. Every setting is guarded by a
@tech/reference{contract}.

@racketinput[(XIDEN_VERBOSE 'yes)]
@racketresult{expected: (or/c void? boolean?)}
@racketinput[(XIDEN_VERBOSE)]
@racketresult[#f]
@racketinput[(XIDEN_VERBOSE #t)]
@racketinput[(XIDEN_VERBOSE)]
@racketresult[#t]

Notice that each setting accepts @racket[(void)]. This has the effect
of removing the value you set in the session. You can use this to undo
a mistake or leave an existing value unchanged on disk.

@racketinput[(XIDEN_VERBOSE (void))]
@racketinput[(XIDEN_VERBOSE)]
@racketresult[#f]

To review the values of all settings, call @racket[dump].
You'll see a hash of the complete current configuration.

@racketinput[(dump)]

When you are satisfied with your changes, call @racket[save!].  This
will write all current settings to your @tech{workspace}'s
configuration file.

@racketinput[(save!)]
