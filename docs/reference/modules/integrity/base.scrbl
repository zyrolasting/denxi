#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    xiden/message
                    xiden/integrity
                    xiden/integrity/base
                    xiden/crypto
                    xiden/source]
         "../../../shared.rkt"]

@title{Integrity Checking Primitives}

@defmodule[xiden/integrity/base]

Unlike @racketmodname[xiden/integrity],
@racketmodname[xiden/integrity/base] provides fundamental definitions
for integrity checking and @tech{cryptographic hash functions} without
concern for the implementation of said functions.


@defstruct*[integrity ([chf-symbol any/c] [digest any/c])]{
Represents integrity information for content. Content passes an
@tech{integrity check} if it, when applied to the CHF named by
@racket[chf-symbol], produces @racket[digest].

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


@defproc[(check-integrity [#:trust-bad-digest trust-bad-digest any/c]
                          [trust-chf? (-> symbol? any/c)]
                          [chf-name symbol?]
                          [actual-digest bytes?]
                          [expected-digest bytes?])
                          symbol?]{
Returns

@itemlist[
@item{@racket['pass] if the digests are @racket[equal?] and the user trusts the CHF.}
@item{@racket['fail] if the digests are not @racket[equal?], but the user trusts the CHF.}
@item{@racket['skip] if @racket[trust-bad-digest] is a true value.}
@item{@racket['curb] if the user does not trust the CHF.}
]
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

A @deftech{cryptographic hash function}, or @deftech{CHF}, is an
algorithm that consumes bytes to produced a fixed-length byte string
called a @deftech{message digest}, or simply @deftech{digest}.


@defstruct*[chf ([canonical-name symbol?]
                 [alias-pattern regexp?]
                 [implementation chf-impl/c])]{
@racket[chf] represents an implementation of a @tech{CHF}, and the
many names it might go by.

@racket[canonical-name] represents the primary name of the CHF in the
context of the running process. @racket[alias-pattern] captures
variations of the canonical name. @racket[implementation] produces a
@tech{digest} using the CHF's specification.

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
Represents a high level implementation of a @tech{CHF}.

The procedure must return a byte string representing a @tech{digest}.

The first argument is an input port that produces bytes from an
arbitrary source, such as an HTTP response or a file. The output
digest must represent @italic{all} bytes drawn from the port.
}


@defthing[current-chfs (parameter/c (listof chf?))]{
The CHFs trusted by the user for digest creation.

Defaults to @racket[null], which disables digest creation and
@tech{integrity checks}.
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
