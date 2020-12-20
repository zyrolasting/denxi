#lang scribble/manual

@require[@for-label[xiden/string racket/base racket/contract]]

@title{String Processing}

@defmodule[xiden/string]

@racketmodname[xiden/string] extends and reprovides @racketmodname[racket/string].

This module defines useful regular expressions and string operations needed by
other modules.

@defproc[(whole/pattstr [s string?]) string?]{
Returns @racket[(string-append "^" s "$")].
}

@defproc[(group/pattstr [s string?]) string?]{
Returns @racket[(string-append "(" s ")")].
}

@defproc[(or/pattstr [opt string?] ...) string?]{
Returns @racket[(string-append "(?:" (string-join opts "|") ")")].
}

@defproc[(make-extension-pattern-string [exts string?] ...) string?]{
Returns a pattern string suitable for @racket[pregexp] that matches
a dot, followed by one of the given @racket[exts] at the end of the subject.
}

@defthing[unix-reserved-character-pattern-string string?]{
A pattern string suitable for @racket[pregexp] that
matches @litchar|{/}| or null characters.
}

@deftogether[(
@defthing[windows-reserved-character-pattern-string string?]
@defthing[windows-reserved-name-pattern-string string?]
)]{
These are pattern strings suitable for @racket[pregexp].
They track @hyperlink["https://docs.microsoft.com/en-us/windows/win32/fileio/naming-a-file"]{Windows reserved file names}.

@racket[windows-reserved-character-pattern-string] matches any character
reserved by Windows for file
names. @racket[windows-reserved-name-pattern-string] matches any character
sequence that looks like a reserved Windows file name.
}


@defthing[maybe-spaces-pattern-string string?]{
A pattern string suitable for @racket[pregexp]
that matches zero or more whitespace characters.
}

@defthing[non-empty-bytes? predicate/c]{
Returns @racket[#t] when applied to a non-empty byte string.
}

@defproc[(make-rx-matcher [pattern-string string?] [#:whole whole? any/c #t]) (-> string? (or/c #f list?))]{
Returns a procedure. That procedure returns @racket[(and (string? s)
(regexp-match (pregexp (if whole? (whole/pattstr p) p))) s)] when applied to a
string @racket[s].
}

@defthing[file-name-string? predicate/c]{
Returns @racket[#t] if the value is a string, and that string is useable as a
cross-platform file name.
}

@defproc[(get-shortest-string [strings (listof string?)]) string?]{
Returns the shortest element of @racket[strings].

Current use cases:

@itemlist[
@item{Show the user the shortest CLI flag among alternatives in some messages.}
]
}


@defproc[(string->value [s string?]) any/c]{
Equivalent to @racket[(read (open-input-string s))].

Current use case:

@itemlist[
@item{
Convert user-provided runtime configuration values to Racket values.
}
]

}

@defidform[#:kind "syntax-class" non-empty-string]{
A syntax class for matching non-empty strings in macros.
}
