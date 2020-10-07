#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base racket/contract xiden/signature]]

@title{Signature Checking}

@defmodule[xiden/signature]

@racketmodname[xiden/signature] authenticates the source of
@tech{package inputs}.

@defstruct*[signature-info ([pubkey (or/c bytes? string?)] [body (or/c bytes? string?)])]{
Holds an expression of a public key file and the bytes of a signature created
using a private key.

When either field is a byte string, then that byte string is used as-is during
a signature verification. This allows one to embed necessary data into an instance
of this structure. If a field is a Racket string, then the bytes are drawn from
a local file or using HTTP depending on the format of the string.
}

@defproc[(signature [pubkey (or/c bytes? string?)] [body (or/c bytes? string?)]) signature-info?]{
An abbreviated @racket[signature-info] constructor for use in @tech{package definitions}.
}

@defproc[(bind-trusted-public-keys [trusted-keys (listof well-formed-integrity-info/c)]) (-> path-string? boolean?)]{
Returns a procedure @racket[P], such that @racket[(P
"/path/to/key.pem")] (for example) is @racket[#t] if the given key
passes an integrity check for one of the @racket[integrity-info]
structures in @racket[trusted-keys].

This is used internally to determine a user's trust in a key
before using a @tech{package input}.
}

@defthing[well-formed-signature-info/c flat-contract?]{
Recognizes an instance of @racket[signature-info] that is suitable for use in signature checking.
}

@defproc[(get-public-key-path [variant (or/c string? bytes?)]) path?]{
Returns a @tech{workspace}-relative path to a public key file. The file may be cached.
}

@defproc[(check-signature [bytes?] [path-string?] [(or/c path-string? bytes?)]) boolean?]{

}

@defproc[(consider-trust [#:trust-bad-digest trust-bad-digest any/c]
                         [siginfo any/c]
                         [continue (-> any/c any)])
                         any]{

}

@defproc[(consider-unsigned [#:trust-unsigned trust-unsigned any/c]
                            [siginfo any/c]
                            [continue (-> any/c any)])
                            any]{

}

@defproc[(consider-public-key-trust #:trust-public-key? [trust-public-key? (-> path-string? any/c)]
                                    [siginfo well-formed-signature-info/c]
                                    [continue (-> path-string? well-formed-signature-info/c any)]
                                    any)]{

}

@defproc[(consider-signature-info
                [public-key-path path-string?]
                [intinfo well-formed-integrity-info/c]
                [siginfo well-formed-signature-info/c]
                (or/c (λ (e) (or (eq? e $signature-verified)
                                 (eq? e $signature-mismatch)))))]{

}
