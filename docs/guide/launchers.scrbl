#lang scribble/manual

@require["../shared.rkt"
         @for-label[racket/base
                    xiden/cli
                    xiden/codec
                    xiden/integrity
                    xiden/signature]]

@title[#:tag "launchers"]{Launchers}

In this writing, a @deftech{launcher} is an app that uses Xiden. We'll
talk about Xiden's built-in launcher, make our own, and then use our
launcher to install software from the definition in @secref{new-pkg}.


@section{Built-in Launcher}

Xiden's built-in launcher is called @litchar{xiden}. Try not to
confuse the names. One is the name of the project, the other is a
command that you run.

Run @litchar{xiden} now, and remember what the output looks like.

Xiden observes @deftech{zero-trust architecture} (“@deftech{ZTA}”)
principles. It refuses to do anything you did not explicitly allow, so
it is a @deftech{zero-trust launcher}.

@litchar{xiden} is also non-interactive, so you have to specify more
than you would expect in command-line flags or environment variables.
We will make our own launcher because it allows us to “bake in”
settings to simplify the interface.

Distribute custom launchers using the built-in launcher to bootstrap
your own rules for software distribution.


@section{Make a Custom Launcher}

Create @litchar{my-xiden.rkt} with this code.

@racketmod[#:file "my-xiden.rkt"
xiden/launcher

(module+ main (launch-xiden!))
]

Now use Racket to start your launcher.

@verbatim|{
$ racket my-xiden.rkt
}|

Notice that the output is the same as running @litchar{xiden}.  That's
because this program @italic{is} the implementation of
@litchar{xiden}! It is a @tech{zero-trust launcher} because it makes
no change to the default settings. The difference is that we fully
control Xiden with trusted code. [@topic{launcher-security}]


@section[#:tag "do"]{Install Software}

The @litchar{do} command installs software in a transaction.

@verbatim[#:indent 2]|{
$ racket my-xiden.rkt do ++install-abbreviated definition.rkt
}|

This transaction performs one installation. If you use the definition
we wrote earlier in the guide, then you'll see @litchar{No
cryptographic hash functions installed} in the output. [@topic{integrity}]

Open your @tech{launcher} and paste in this code.

@racketblock[
(require file/sha1)
(current-chfs (list (chf 'sha1 #px"^(?i:sha[-_]?1)$" sha1-bytes)))
]

This defines a SHA-1 name, a pattern for accepted aliases, and an
implementation.  That way we can use the same function when we run
into @racket['sha1], @racket['SHA-1], or @racket['Sha_1] symbols.

Run the installation again. You'll see some output ending in

@verbatim[#:indent 2]|{
signature check: untrusted public key
To trust this key, add this to XIDEN_TRUST_PUBLIC_KEYS:
(integrity 'sha1 (base64 #"MKFCv/e0xEtZME35JRflXixsxyE="))
}|

This means we have to trust the public key used to verify a
signature. [@topic{signature}]

Copy this code to your launcher.

@racketblock[
(XIDEN_TRUST_PUBLIC_KEYS
  (list (integrity 'sha1 (base64 #"MKFCv/e0xEtZME35JRflXixsxyE="))))
]

When you are ready, run @litchar{racket my-xiden.rkt do +a
definition.rkt} again. You'll see a symbolic link appear in the
current directory called @tt{my-first-package}. You did it!

To uninstall the software, delete the link and collect garbage
using the @litchar{gc} command.

@verbatim[#:indent 2]|{
$ rm my-first-package
$ racket my-xiden.rkt gc
}|
