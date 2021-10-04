#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    denxi/message
                    denxi/integrity
                    denxi/integrity/base
                    denxi/crypto
                    denxi/source]
         "../../../shared.rkt"]

@title{Integrity Checking Primitives}

@defmodule[denxi/integrity/base]

Unlike @racketmodname[denxi/integrity],
@racketmodname[denxi/integrity/base] provides fundamental definitions
for integrity checking.


@defstruct*[integrity ([chf-symbol any/c] [digest any/c])]{
Represents integrity information for content. An instance passes
@racket[check-integrity] if data processed by the CHF named by
@racket[chf-symbol] maps to @racket[digest]. An instance does not
indicate which implementation of a CHF to use.

The fields may be any value to address particular scenarios.  Use
@racket[raw-integrity?], @racket[well-formed-integrity?],
@racket[malformed-integrity?], and @racket[sourced-integrity?] to
classify instances.
}


@defthing[raw-integrity? flat-contract? #:value (struct/c symbol? bytes?)]{
Returns @racket[#t] if the argument is an instance of
@racket[integrity] has a symbolic CHF name and an unencoded digest
available in the instance itself.

This contract does not validate the content of the fields, but an
instance that passes this contract is suitable for use with
@racket[check-integrity].
}


@margin-note{Allowing @racket[#f] in the arguments is intentional due
to the possibility of missing information.}
@defproc[(check-integrity [#:trust-bad-digest trust-bad-digest any/c]
                          [trust-chf? (-> symbol? any/c)]
                          [int (or/c #f integrity?)]
                          [expected-digest (or/c #f bytes?)])
                          symbol?]{
Returns

@itemlist[
@item{@racket['digests-match] if the digests are @racket[equal?] and the user trusts the CHF.}
@item{@racket['digests-differ] if the digests are not @racket[equal?], but the user trusts the CHF.}
@item{@racket['skip] if @racket[trust-bad-digest] is a true value.}
@item{@racket['blocked-chf] if the user does not trust the CHF.}
@item{@racket['malformed-input] if @racket[(raw-integrity? int)] is @racket[#f].}
]
}


@defthing[integrity-check-passed? predicate/c]{
Returns @racket[#t] if the argument is in the range of
@racket[check-integrity], and you can interpret it as permission to
proceed in a larger procedure.
}


@defthing[MAX_EXPECTED_DIGEST_LENGTH exact-positive-integer?]{
The maximum number of bytes expected from any message digest.
}


@defproc[(make-digest [in (or/c bytes? path-string? input-port?)]
                      [chf-name (or/c #f symbol?) (get-default-chf)])
                      bytes?]{
Returns the unencoded bytes for a message digest computed using
@racket[chf], using bytes drawn from a given input port.

If @racket[in] is a path string, it is coerced to an input port using
@racket[call-with-input-file*].

If @racket[in] is a byte string, it is coerced to an input port using
@racket[open-input-bytes].

If @racket[chf-name] is @racket[#f], or no CHF implementation is
available for it, @racket[make-digest] will @racket[raise]
@racket[($chf-unavailable chf-name)].
}


@section{Cryptographic Hash Functions}

@defstruct*[chf ([canonical-name symbol?]
                 [alias-pattern regexp?]
                 [implementation chf-impl/c])]{
@racket[chf] represents an implementation of a cryptographic hash
function, and the many names it might go by.

@racket[canonical-name] represents the primary name of the CHF in the
context of the running process. @racket[alias-pattern] captures
variations of the canonical name. @racket[implementation] produces a
digest using the CHF's specification.

An instance might encode SHA-1 as follows:

@racketblock[
(chf 'sha1 #px"^(?i:sha[-_ ]?1)$" sha1-bytes) (code:comment "openssl/sha1, file/sha1")
]

The name fields capture possible variation where CHF symbols appear,
such as in @racket[integrity] instances.

@racketblock[
(integrity 'SHA1 #"...")
(integrity 'SHA-1 #"...")
(integrity 'Sha_1 #"...")
(integrity 'sha1 #"...") (code:comment "<- canonical")
(integrity 'sha-1 #"...")
]

@racket[chf] implements @racket[prop:procedure], such that applying
the instance to arguments will forward those arguments to the
procedure bound to the @racket[implementation] field.

@racket[chf] implements @racket[gen:equal+hash], such that two
instances are @racket[equal?] if their canonical names are
@racket[eq?].
}


@defstruct*[$chf-unavailable ([name symbol?]) #:prefab]{
The process could not find an instance of @racket[chf] using
@racket[name].
}


@defthing[chf-impl/c
          chaperone-contract?
          #:value (-> input-port? bytes?)]{
Represents a high level implementation of a cryptographic hash
function.

The procedure must return a byte string representing an unencoded
digest.

The first argument is an input port that produces bytes from an
arbitrary source, such as an HTTP response or a file. The output
digest must represent @italic{all} bytes drawn from the port.
}


@defthing[current-chfs (parameter/c (listof chf?))]{
The CHFs trusted by the user for digest creation.

Defaults to @racket[null], which disables digest creation and
integrity checks.
}


@defproc[(chf-bind-trust [chfs (listof chf?) (current-chfs)]) predicate/c]{
Returns a predicate @racket[P], such that @racket[(P v)] is
@racket[#t] if @racket[v] is a symbol and @racket[(chf-find chfs v)]
returns a non-@racket[#f] value.
}

@defproc[(chf-fold-trust [select-implementation (-> symbol? chf-impl/c)]
                         [trusted-chf-names (listof symbol?)])
                         (listof chf?)]{
Returns a list of @racket[chf] instances based on trusted names.

For each @racketid[name] in @racket[trusted-chf-names], the output
list will contain:

@racketblock[
(chf name
     (pregexp (format "^(?i:~a)$" name))
     (select-implementation name))]
}


@defproc[(get-default-chf [trusted (listof chf?) (current-chfs)]) (or/c #f symbol?)]{
Returns the canonical name of the first element in @racket[trusted],
or @racket[#f] if @racket[trusted] is empty.
}


@defproc[(chf-find [haystack (listof chf?)] [needle symbol? (get-default-chf haystack)])
         (or/c #f chf?)]{
Returns the first @racket[chf] instance in @racket[haystack] where
@racket[needle] either exactly matches the canonical name of the
instance, or matches its alias pattern.

Returns @racket[#f] if no instance meets the criteria.
}
