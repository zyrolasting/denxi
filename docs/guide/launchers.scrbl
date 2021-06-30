#lang scribble/manual

@require["../shared.rkt"
         @for-label[racket/base
                    xiden/cli
                    xiden/codec
                    xiden/integrity
                    xiden/signature]]

@title[#:tag "launchers"]{Launchers}

A @deftech{launcher} is an app that starts and uses Xiden. We launch
Xiden's built-in launcher with the @litchar{xiden} command. Run it now
with no arguments, and remember what the output looks like.

Xiden observes @deftech{zero-trust architecture} (“@deftech{ZTA}”)
principles. It refuses to do anything you did not explicitly allow, so
it is a @deftech{zero-trust launcher}. This makes @litchar{xiden}
intensely bereaucratic, so make Xiden easier to use with a custom
launcher. Save this code to a new @litchar{my-xiden.rkt} launcher
file.

@racketmod[#:file "my-xiden.rkt"
xiden/launcher

(module+ main (launch-xiden!))
]

The output of @litchar{racket my-xiden.rkt} is the same as just
@litchar{xiden}. They make no change to default settings, so they are
both @tech{zero-trust launchers}. The difference is that in our
launcher, we can control Xiden with trusted code
[@topic{launcher-security}]. We'll start with the same verbose
commands as the built-in launcher, and simplify it as we go.

To install software, we'll give the package definition file
@litchar{definition.rkt} to the @litchar{do} command. @litchar{do}
runs a transaction against your file system. The word “transaction”
essentially means “Install everything I give you. If there's any
problem, put everything back the way it was.”

@verbatim[#:indent 2]|{
$ racket my-xiden.rkt do ++install-abbreviated definition.rkt
}|

If you use the definition we wrote earlier in the guide, then you'll
see @litchar{No cryptographic hash functions installed} in the output
[@topic{integrity}]. To fix this, open your @tech{launcher} and paste
in this code.

@racketblock[
(require file/sha1)
(current-chfs (list (chf 'sha1 #px"^(?i:sha[-_]?1)$" sha1-bytes)))
]

@margin-note{SHA-1 is a poor choice for security-critical code, which
this guide is not. In fact, Xiden provides that same @racket[chf]
value as @racket[snake-oil-chf] to discourage misuse.}

This defines a SHA-1 name, a pattern for accepted aliases, and an
implementation.  That way we can use the same function when we run
into @racket['sha1], @racket['SHA-1], or @racket['Sha_1] symbols.

Run the installation again. You'll see some output ending in

@verbatim[#:indent 2]|{
signature check: untrusted public key
To trust this key, add this to XIDEN_TRUST_PUBLIC_KEYS:
(integrity 'sha1 (base64 #"MKFCv/e0xEtZME35JRflXixsxyE="))
}|

Now we have to trust the public key used to verify a signature
[@topic{signature}]. It's from a keypair made specifically for this
tutorial, so you don't need to trust it for more than a few
seconds. Copy this code to your launcher.

@racketblock[
(XIDEN_TRUST_PUBLIC_KEYS
  (list (integrity 'sha1 (base64 #"MKFCv/e0xEtZME35JRflXixsxyE="))))
]

Run @litchar{racket my-xiden.rkt do +a definition.rkt} one last time,
and you'll see a symbolic link appear that points to your installed
software.

To uninstall the software, delete the link and collect garbage
using the @litchar{gc} command.

@verbatim[#:indent 2]|{
$ rm my-first-package
$ racket my-xiden.rkt gc
}|

Notice that this entire time we never changed the install command.
Our launcher filled in gaps that @litchar{xiden} would expect as
command-line flags.

@verbatim[#:indent 2]|{
$ xiden do ++install-abbreviated definition.rkt \
        ++XIDEN_TRUST_CHFS sha1 \
        ++XIDEN_TRUST_PUBLIC_KEYS \
        '(integrity \'sha1 (base64 #"MKFCv/e0xEtZME35JRflXixsxyE="))'
}|

The more configuration you add, the simpler the default CLI becomes.
After sufficient work, you can start replacing entire Xiden commands
with equivalents that make sense for you and your target audience.
