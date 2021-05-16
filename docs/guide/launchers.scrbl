#lang scribble/manual

@require["../shared.rkt"
         @for-label[racket/base
                    xiden/cli]]

@title[#:tag "launchers"]{Launchers}

If you haven't worked through @secref{new-pkg}, please do so. We'll
use the @tt{definition.rkt} file from that section here.

To control Xiden, we'll create a @bold{trusted} (see
@secref{launcher-security}) Racket module to act as a
@deftech{launcher}.

@racketmod[#:file "my-xiden.rkt"
xiden/launcher

(module+ main (launch-xiden!))
]

We use the main submodule to install Xiden's command line interface.
we can now use this module to launch Xiden.

@verbatim|{
$ racket my-xiden.rkt
}|

On Unix-like systems, you can use a shebang like @litchar|{#!
/usr/bin/env racket}|. You can then execute the launcher directly for
the same effect, with a shorter command.

@verbatim|{
$ mv my-xiden.rkt xi
$ chmod +x xi
$ PATH="$PATH:$PWD"
$ xi
}|

Unlike other dependency managers, Xiden's command-line interface can
be made subservient to a user's launcher. This allows more
customizations, and you can substitute the entire CLI if you have
specific needs.

I will refer to the above @litchar{xi} command when walking through
the commands. We will add to the launcher's code as we progress
through this section.

@section[#:tag "do"]{Running Our First Transaction}

Xiden uses transactions. That way when it fails, it can still start
from a working state later. To run a transaction, use the @litchar{do}
command.

@verbatim|{
$ xi do +a definition.rkt
}|

This command defines a transaction with one step: To install something
from a definition. If you use the definition we wrote earlier in the
guide, then this command will fail with the following message in the
report:

@verbatim|{
default.tgz: integrity violation: not trusting CHF sha384. To bypass, add it to XIDEN_TRUST_CHFS
}|

Remember that we defined integrity information using a SHA-384
digest. Xiden is paranoid, and will not proceed with any operation
that it cannot trace back to your affirmative consent. We didn't say
that we trusted SHA-384 digests, so Xiden rejects the input.

Open the @tech{launcher} and add this line.

@racketblock[
(XIDEN_TRUST_CHFS '(sha384))
]

This tells Xiden that you trust SHA-384. Because this value is
programmed directly into the launcher, it will apply every time you
run it.

Run @litchar{xi do +a definition.rkt} again for a different
message.

@verbatim|{
default.tgz: signature violation: public key not trusted. To trust this key, add this to XIDEN_TRUST_PUBLIC_KEYS:
(integrity 'sha384 (base64 "n2Ac8K56quwznmSJFZZtnZFxL1ck16hUf+Ule2jd1bHGMJy/EiK2Vc2ibCITnyM0"))
}|

Not a bug, but a feature. Xiden refused to use an input because you
never said that you trusted the public key used to verify
@racket{default.tgz}'s signature. It's one of my keys. If you trust it
enough to continue, copy the @racket[integrity] expression to your
clipboard add this code to your launcher.

@racketblock[
(XIDEN_TRUST_PUBLIC_KEYS
  (list (integrity 'sha384
                   (base64 "n2Ac8K56quwznmSJFZZtnZFxL1ck16hUf+Ule2jd1bHGMJy/EiK2Vc2ibCITnyM0"))))
]


This back and forth is intended to encourage explicit consent to
specifics starting from no trust, just like a well-designed router.
If this is too inconvenient for you, Xiden can be adjusted to offer
trust over an entire topic. This is not a good habit, though. You
should leverage the zero-trust configuration to interactively add
trust only for what you expressly want. This takes more work, but it
helps keep you safe, and Xiden's error messages will guide you.

When you are ready, run @litchar{xi do +a definition.rkt} again. If
you see a symbolic link appear in the current directory called
@tt{my-first-package}, then you did it!


@subsection{What's with the Link?}

The @litchar{+a} switch is actually short for
@litchar{++install-abbreviated}.  You give it a @tech{package
definition} and it will build the @racket{default} @tech{package
output}. It will then issue you a link to the output directory named
after the package.

Xiden will remember the link. Think of the link as being
@italic{bound} to a directory just like an identifier is bound to a
value in Racket. When you no longer need the output, remove the link.

@verbatim|{
$ rm my-first-package
}|

All files without links are eligible for garbage collection.

@verbatim|{
$ xi gc
}|

And with that, you now know how to uninstall things.

@subsection[#:tag "abbrev"]{Tweaking Transaction Flags}

You may be interested to know that all of these commands are equivalent:

@verbatim|{
# long flags
$ xi do ++install-source my-first-package default definition.rkt
$ xi do ++install-default my-first-package definition.rkt
$ xi do ++install-abbreviated definition.rkt

# short flags
$ xi do +s my-first-package default definition.rkt
$ xi do +d my-first-package definition.rkt
$ xi do +a definition.rkt
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
$ xi do ++install-source ... ++install-abbreviated ... ++install-source ...
}|

Don't worry about installing conflicting versions. Xiden installs
dependencies side-by-side. If you try to install the same thing twice,
then Xiden will reuse the existing directory.


@subsection{Creating Arbitrary Links}

Sometimes it makes sense to make links to specific files in a package's
output. A good way to do this is to create a link as normal using a relative
path that follows a link you've already created using @litchar{xi do}.

@verbatim|{
$ xi do +d vendor definition.rkt
$ ln -s vendor/my-first-package/main.rkt my-first-package.rkt
}|

The link created using your operating system is not tracked by Xiden,
so a garbage collection pass can break the link. But when you use a relative
path as shown, then you can repair the link by running the same transaction.


@section{Fetching Sources}

You can use Xiden's data fulfilment features with the @litchar{fetch}
command. The @litchar{fetch} command evaluates a given
@tech/xiden-reference{source} expression and writes bytes to standard
output.  The entire process is subject to the current
@tech/xiden-reference{runtime configuration}.

@verbatim|{
$ xi fetch '(http-source "https://example.com/file.tgz")' >file.tgz
}|

You can use the @litchar{fetch} command as a downloader, or as a way
to verify if data fulfilment works on a source under your settings.

@litchar{fetch} does not check the integrity or signature of the
output data.


@section{Printing Reports}

Use the @litchar{show} command to review key information.

@litchar{xi show installed} shows all installed outputs.  Each line
contains a @tech{package query} matching an exact @tech{package
definition} used on your system, a name for an output used with that
definition, and a path where the output is located.

@litchar{xi show links} shows all records of symbolic links issued by
Xiden for the @tech/xiden-reference{target workspace}. Each line is
formatted as @litchar{L -> T}, where @litchar{L} is a path to a
symbolic link, and @litchar{T} is a path to the linked file on disk.
Either path might be a relative path. If it is, then that path is
relative to a @tech/xiden-reference{target workspace}. Note that
@litchar{L} might not exist. @litchar{xi gc} will remove any link
record where the link does not exist at @litchar{L}.


@section[#:tag "gc"]{Collecting Garbage}

We briefly visited the @litchar{gc} command in @secref{do}. We
observed that if you delete a link issued by @litchar{xi do} on
your disk, then @litchar{xi gc} may collect the target.

A file or directory is eligible for garbage collection if it has no
incoming links issued by Xiden.

If you ever create your own symbolic link to a file created by Xiden,
then that link is dependent on the file remaining after any garbage
collection pass.



@section[#:tag "launcher-security"]{Security Implications of Launchers}

Xiden's default configuration can be thought of as “Deny All”, but a
custom launcher is any Racket module that starts with all privileges
granted by the operating system. Any restrictions Xiden sets in terms
of the configuration will only apply when the launcher actually gives
control to Xiden. So, if your custom launcher is compromised, then
Xiden is compromised.

You can use the built-in @litchar{xiden} launcher to mitigate some
risks. It has the same interface as other launchers.

@verbatim|{
$ xiden do +d vendor definition.rkt
}|

The key difference is that the @litchar{xiden} launcher does not
modify some @tech/reference{parameters}, like
@racket[current-package-editor]. @litchar{xiden} is only configurable
by command-line flags and environment variables, which reduces the
size of the attack surface.

So why would you ever use a custom launcher? Custom launchers can do
things that the default launcher cannot, such as intelligently respond
to package conflicts, extend the CLI, or resolve @tech{package
queries} in specific ways. The good news is that launchers can
themselves be signed and distributed using Xiden, to protect trust in
how Xiden launches despite the customizations.

To learn more, see @secref[#:doc xiden-tutorials]{securing-launch}.
