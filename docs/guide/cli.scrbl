#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title[#:tag "cli"]{Command Line Interface}

Xiden offers two entry points to its command-line interface.  One is a
launcher named @tt{xiden}, which @tt{raco pkg} (or Xiden!) creates
during setup. You can also use the command line through the @litchar{raco
zcpkg} (“zero-collection package”) command if only @litchar{raco} is in your
@tt{PATH}.  While @tt{raco zcpkg} and @tt{xiden} both launch the same program, I
will only refer to it as @tt{xiden} from now on.

@tt{xiden} is a non-interactive program, although it can allow
interactions with subprocesses. Its configuration is held constant
after launch, and every one of its settings can be defined at the
command line.  This way each command can be expressed as a pure
function.

@tt{xiden} commands are explicit and verbose. If typing a command becomes
tedious, then please leverage the features of your shell and review
the @secref{config} section.


@section[#:tag "do"]{Running Our First Transaction}

If you haven't worked through @secref{new-pkg}, please do so. We'll use the
@tt{definition.rkt} file from that section here.

Xiden writes files to your disk using transactions. This means when it
fails, your system will be left as it was before the transaction started. To
run a transaction, use the @litchar{do} command.

@verbatim|{
$ xiden do +a definition.rkt
}|

This command defines a transaction with one step: To install something from a
definition. If you are using the definition from @secref{finished-definition}
on a new Xiden installation, then this command will fail with the
following message in the report:

@verbatim|{
default.tgz: https://sagegerard.com/xiden-tutorial/default.tgz: signature violation: public key not trusted. To trust this key, add this to XIDEN_TRUSTED_PUBLIC_KEYS:
(integrity 'sha384 (base64 "n2Ac8K56quwznmSJFZZtnZFxL1ck16hUf+Ule2jd1bHGMJy/EiK2Vc2ibCITnyM0"))
}|

Xiden is paranoid. It will not proceed with any operation that it cannot trace
back to your affirmative consent. This message is telling you that it refused
to use an input because you never said that you trusted the public key used to
verify @racket{default.tgz}'s signature.

As an exercise, copy the @racket[integrity] expression to your clipboard and
use @secref{config} to edit your configuration. When you are ready, run
@litchar{xiden do +a definition.rkt} again. If you see a symbolic link appear
in the current directory called @tt{my-first-package}, then you did it!


@section{What's with the Link?}

The @litchar{+a} switch is actually short for @litchar{++install-abbreviated}.
You give it a @tech{package definition} and it will build the @racket{default}
@tech{package output}. It will then issue you a link to the output directory
named after the package.

This is a key difference between systems like Xiden and traditional
package managers. It doesn't keep a central repository of names. It keeps
a central repository of unique files and directories, and gives you links
to the things you need with names that you choose.

The link is also special in that Xiden remembers making it. Think of
the link as being @italic{bound} to a directory just like an identifier is
bound to a value in Racket. When you no longer need the output, remove the
link.

@verbatim|{
$ rm my-first-package
}|

All files without links are eligible for garbage collection.

@verbatim|{
$ xiden gc
}|

And with that, you now know how to uninstall things.

@subsection[#:tag "abbrev"]{Tweaking Transaction Flags}

You may be interested to know that all of these commands are equivalent:

@verbatim|{
# long flags
$ xiden do ++install-source my-first-package default definition.rkt
$ xiden do ++install-default my-first-package definition.rkt
$ xiden do ++install-abbreviated definition.rkt

# short flags
$ xiden do +s my-first-package default definition.rkt
$ xiden do +d my-first-package definition.rkt
$ xiden do +a definition.rkt
}|

@litchar{++install-default} is like @litchar{++install-abbreviated} except you
get to control where you create a link, and @litchar{++install-source} let's
you also control what package output to install.

This creates a spectrum where longer commands offer more flexibility. You never
have to worry about file conflicts if you control the exact namespace of
dependency references, and you never have to worry about bloat if you control
what exact deliverable each package places on your disk.


@subsection{Installing Multiple Packages}

You can specify more than one definition to install in order.  Note
that one @litchar{do} command equals one transaction! In this context,
three installations occur in one transaction. If anything goes wrong
in the third installation, then the entire transaction fails to the
state before the command ran.

@verbatim|{
$ xiden do ++install-source ... ++install-abbreviated ... ++install-source ...
}|

Don't worry about installing conflicting versions. @tt{xiden} installs
dependencies side-by-side. If you try to install the same package
twice, then @tt{xiden} will reuse the existing directory.


@subsection{Creating Arbitrary Links}

Sometimes it makes sense to make links to specific files in a package's
output. A good way to do this is to create a link as normal using a relative
path that follows a link you've already created using @litchar{xiden do}.

@verbatim|{
$ xiden do +d vendor definition.rkt
$ ln -s vendor/my-first-package/main.rkt my-first-package.rkt
}|

The link created using your operating system is not tracked by Xiden,
so a garbage collection pass can break the link. But when you use a relative
path as shown, then you can repair the link by running the same transaction.


@section{Printing Reports}

Use the @litchar{show} command to review key information.

@subsection{View Path to Workspace}

@litchar{xiden show workspace} shows the path to the @tech{target
workspace}. See @secref{workspace} for more information on how Xiden
defines this path.

@subsection{View Installed Outputs}

@litchar{xiden show installed} shows all installed outputs.  Each line contains
a @tech{package query} matching an exact @tech{package definition} used on your
system, a name for an output used with that definition, and the path where the
output is located in the @tech{target workspace}.

@subsection{View Issued Links}

@litchar{xiden show links} shows all records of symbolic links issued by
Xiden for the @tech{target workspace}. Each line is formatted as
@litchar{L -> T}, where @litchar{L} is a path to a symbolic link, and
@litchar{T} is a path to the linked file on disk.

Either path might be a relative path. If it is, then that path is relative to
the @tech{target workspace}.

Note that @litchar{L} might not exist. @litchar{xiden gc} will remove any link
record where the link does not exist at @litchar{L}.

@subsection{View Runtime Configuration}

@litchar{xiden show config} shows the runtime configuration for the
@tech{target workspace}. Use this to verify the configuration that applies
before factoring in any command line arguments.


@section[#:tag "cli-overrides"]{Overriding Inputs}

The beauty of @tech{package inputs} is that they can be overridden.
One way to do this is to use override flags.

@verbatim|{
$ xiden do +a definition.rkt +o '^leagues:baseball' '(input "umpire" (integrity ...))'
}|

An override applies to all packages in the scope of a transaction.
This override takes two arguments. The first is a readable regular
expression that matches against a @tech{package query}. The second
is the code for an input expression.

When Xiden processes packages, it will build a package query using
only the provider, package name, edition and revision number
(e.g. @racket{leagues:baseball:pro:89}). If the pattern matches,
then the input expression provided in the command replaces the
input expression of the same name in that package.

The override applies to all eligible packages, and all inputs of the
same name. This allows you to standardize dependencies in the event
you end up with something like multiple slightly different copies of
Ruby.

@verbatim|{
$ RUBY='(input "ruby" (integrity ...))'
$ xiden do +a definition.rkt \
  +o 'syntax-highlighting' "$RUBY" \
  +o 'images' "$RUBY"
}|


@section[#:tag "gc"]{Collecting Garbage}

We briefly visited the @litchar{gc} command in @secref{do}. We
observed that if you delete a link issued by @litchar{xiden do} on
your disk, then @litchar{xiden gc} may collect the target.

A file or directory is eligible for garbage collection if it has no
incoming links issued by Xiden.

The garbage collector follows a three step process:

@itemlist[#:style 'ordered
@item{Forget about any links where a link file does not actually exist on disk.}
@item{Delete all files or directories in the @tech{target workspace}'s @litchar{var/xiden/objects} directory with no incoming links.}
@item{If nothing was actually deleted in Step 1 or Step 2, print the number of bytes recovered. Otherwise, go to Step 1.}
]

Note that only the immediate contents (as in @racket[directory-list]) of a
@tech{workspace}'s @litchar{var/xiden/objects} directory are monitored, because
Xiden @italic{does not issue links to the contents of any
subdirectory}. This means that if you ever create a symbolic link to a file
inside of a package's output directory, then that link is dependent on the
output directory remaining after any garbage collection pass.
