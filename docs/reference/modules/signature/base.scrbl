#lang scribble/manual

@require["../../../shared.rkt"
         @for-label[racket
                    denxi/message
                    denxi/integrity
                    denxi/signature/base
                    denxi/signature/ffi
                    denxi/source]]

@title{Signature Checking Primitives}

@defmodule[denxi/signature/base]

@defstruct*[signature ([public-key any/c] [body any/c]) #:transparent]{
Represents a claim that the bytes in @racket[body] are a signature
that can be verified using @racket[public-key].
}

@defthing[raw-signature? flat-contract? #:value (struct/c signature bytes? bytes?)]{
A @tech/reference{flat contract} for signature claims that contain
only unencoded bytes in memory.
}

@defthing[make-signature/c
          chaperone-contract?
          #:value (-> bytes?
                      symbol?
                      bytes?
                      (or/c #f bytes?)
                      bytes?)]{
A contract for procedures that return new signatures as unencoded
bytes.

Arguments
@itemlist[#:style 'ordered
@item{A digest as unencoded bytes}
@item{A symbol representing the name of the cryptographic hash function used to create the first argument.}
@item{A private key of some encoding.}
@item{A password for the private key, or @racket[#f] if there is no password.}
]
}

@defthing[verify-signature/c
          chaperone-contract?
          #:value (-> bytes?
                      symbol?
                      bytes?
                      bytes?
                      boolean?)]{
A contract for procedures that return @racket[#t] for trusted
signatures.

Arguments
@itemlist[#:style 'ordered
@item{A digest as unencoded bytes}
@item{A symbol representing the name of the cryptographic hash function used to create the first argument.}
@item{A public key of some encoding.}
@item{An unencoded signature}
]
}


@margin-note{Allowing @racket[#f] in the arguments is intentional due
to the possibility of missing information.}
@defproc[(check-signature [#:trust-public-key? trust-public-key? (-> input-port? any/c)]
                          [#:trust-unsigned trust-unsigned any/c]
                          [#:verify-signature verify-signature verify-signature/c]
                          [#:trust-bad-digest trust-bad-digest any/c]
                          [sig (or/c #f signature?)]
                          [int (or/c #f integrity?)])
                          symbol?]{
@margin-note{@racket['skip] and @racket['skip-unsigned] are not
equivalent. @racket[check-signature] only handles a lack of a
signature when @racket[sig] or @racket[int] is malformed.}

Returns

@itemlist[
@item{
@racket['skip] if the check was skipped.
}

@item{
@racket['signature-verified] when trusting the public key and the signature.
}

@item{
@racket['signature-unverified] when trusting the public key but not the signature.
}

@item{
@racket['blocked-public-key] when distrusting the public key.
}

@item{
@racket['unsigned] if @racket[sig] or @racket[int] are missing too much information to conclude that a signature is present.
}

@item{
@racket['skip-unsigned] is a combination of @racket['skip] and
@racket['unsigned].  @racket[int] and/or @racket[sig] are missing
information, but this is considered permissable when
@racket[trust-unsigned] is @racket[#t].
}

]
}

@defthing[signature-check-passed? predicate/c]{
Returns @racket[#t] if the argument is in the range of
@racket[check-signature], and you can interpret it as permission to
proceed in a larger procedure.
}


@defthing[current-verify-signature
          (parameter/c verify-signature/c)]{
A parameter holding the current way to verify a signature.

If @racket[(signature-ffi-available?!)] is @racket[#t], the default
value is @racket[signature-ffi-verify-signature]. Otherwise,
@racket[(const #f)].
}


@defthing[current-make-signature
          (parameter/c make-signature/c)]{
A parameter holding the current way to make a signature.  The default
value returns an empty byte string.
}
