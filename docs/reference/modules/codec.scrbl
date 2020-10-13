#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    xiden/codec
                    @except-in[xiden/pkgdef #%module-begin]]
         xiden/codec
         racket/pretty
         "../../shared.rkt"]

@title{Codec}

@defmodule[xiden/codec]

The @racketmodname[xiden/codec] module encodes and decodes byte
strings. Note that @tech/reference{ports} are not used, so all data
sits entirely in memory.

@section{High-Level Interface}

@defproc[(encode [encoding xiden-encoding/c] [variant (or/c bytes? string?)]) (or/c bytes? string?)]{
Encodes a byte string or UTF-8 encoded string according to
@racket[encoding]. The return value type matches the type of
@racket[variant].
}

@defproc[(decode [encoding xiden-encoding/c] [encoded (or/c bytes? string?)]) (or/c bytes? string?)]{
Decodes a byte string or UTF-8 encoded string according to
@racket[encoding]. The return value type matches the type of
@racket[encoded].
}

@defthing[xiden-encodings (listof symbol?)]{
A list of supported encodings of byte strings.

Bound to @typeset-code[(pretty-format #:mode 'print xiden-encodings)]
}

@defthing[xiden-encoding/c flat-contract? #:value (apply or/c xiden-encodings)]{
A contract that accepts a value in @racket[xiden-encodings].
}

@defproc[(encoded-file-name [variant (or/c bytes? string?)]) path-string?]{
Returns up to the first 32 characters of @racket[(coerce-string
(encode 'base32 variant))], which is suitable for use as a file name
across platforms.
}

@section{UTF-8 Conversions}

@defproc[(coerce-string [v (or/c string? bytes?)]) string?]{
Equivalent to

@racketblock[(if (string? v) v (bytes->string/utf-8 v))]
}

@defproc[(coerce-bytes [v (or/c string? bytes?)]) bytes?]{
Equivalent to

@racketblock[(if (bytes? v) v (string->bytes/utf-8 v))]
}


@section{Abbreviated Decoding Procedures}

@defthing[abbreviated-decode-procedure/c chaperone-contract? #:value (-> (or/c non-empty-string? bytes?) bytes?)]{

An @deftech{abbreviated decoding procedure} (or @deftech{ADP}) helps a
user represent a byte string with a more convenient notation.

e.g.

@racketblock[
(integrity 'sha384 (base64 "z/2YR1yTDC0YBvgQFpGUzXGtqZdPx+E//C2tyYNDENnLf7NGexuuDc/3yOhoGqBn"))
]

}

@defthing[hex abbreviated-decode-procedure/c]{
Decodes a hex-encoded string to bytes. The process is case-insensitive.

@racket[encoded] may separate each pair of hex digits with @racket{:}.

This implies @racket[(equal? (hex "deadbeef") (hex "de:ad:be:ef"))].
}

@defthing[base64 abbreviated-decode-procedure/c]{
Decodes a Base64-encoded string to bytes.
}

@defthing[base32 abbreviated-decode-procedure/c]{
Decodes a Base32-encoded string to bytes.
}
