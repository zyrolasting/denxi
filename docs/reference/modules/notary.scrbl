#lang scribble/manual

@require[@for-label[racket
                    xiden/artifact
                    xiden/integrity
                    xiden/notary
                    xiden/openssl
                    xiden/signature
                    xiden/source
                    xiden/subprogram]
         "../../shared.rkt"]

@title{Notaries}

@defmodule[xiden/notary]

@defstruct*[notary ([chf chf/c]
                    [public-key-source (or/c #f source-variant?)]
                    [private-key-path (or/c #f path-string?)]
                    [private-key-password-path  (or/c #f path-string?)])]{
A @deftech{notary} is an instance of @racket[notary]. Notaries attach
integrity information and a private party's signature to artifacts.

Unlike many other abstractions in Xiden, notaries depend on secrets to
perform complete work. Those secrets must be available on the file
system.
}

@defproc[(make-notary [#:chf chf chf/c DEFAULT_CHF]
                      [#:public-key-source public-key-source (or/c #f path-string?) #f]
                      [#:private-key-path private-key-path (or/c #f path-string?) #f]
                      [#:private-key-password-path
                       private-key-password-path
                       (or/c #f path-string?)
                       #f])
                       notary?]{
Returns a new @racket[notary], with contract enforcement and default
values for fields.
}

@defthing[lazy-notary notary?]{
A @tech{notary} that only creates integrity information using
@racket[DEFAULT_CHF].
}

@defthing[fraudulent-notary notary?]{
A @tech{notary} that creates complete artifacts, using an
intentionally-leaked keypair sent to your system when installing
Xiden. @racket[(notarize fraudulent-notary trusted-content)] values
are implicitly compromised for all @racket[trusted-content]. Use this
notary only when prototyping signature verification.
}

@defproc[(notarize [the-notary notary?]
                   [trusted-content (or/c artifact-info? source-variant?)])
                   (subprogram/c artifact-info?)]{
Returns a @tech{subprogram} that computes a new @tech{artifact} in
terms of the @tech{source} accessible through
@racket[trusted-content].

The output artifact's data will be in parity with the information
available in @racket[the-notary]: If there is no defined CHF, then the
output artifact will lack integrity and signature information.  If the
notary lacks a complete keypair, then the output artifact will lack
signature information. The output artifact only shares the primary
content @tech{source} accessible from @racket[trusted-content], and
will not validate or use input integrity/signature information.

@racket[trusted-content] is, as the name implies, assumed to be
trusted by the caller. No safety limits will be in place when drawing
bytes from its @tech{source} to compute a digest.

If integrity information @racketid[I] is in the output artifact, then
@racket[(integrity-info-algorithm I)] is @racket[eq?] to
@racket[(notary-chf the-notary)]. @racket[(integrity-info-digest I)]
is the digest computed using @racket[trusted-content].

If signature information @racketid[S] is in the output artifact, then
@racket[(signature-info-pubkey S)] is @racket[eq?] to
@racket[(notary-public-key-source
the-notary)]. @racket[(signature-info-body S)] is a signature computed
using @racket[(integrity-info-digest I)].
}
