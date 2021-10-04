#lang scribble/manual

@require[@for-label[denxi/string racket/base racket/contract]]

@title{Strings}

@defmodule[denxi/string]

@racketmodname[denxi/string] extends and reprovides
@racketmodname[racket/string].

This module defines useful regular expressions and string operations
needed by other modules.

@defthing[DEFAULT_STRING non-empty-string? #:value "default"]{
The conventional default of the non empty strings.
}

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
}

@defproc[(string->value [s string?]) any/c]{
@racket[read]s a Racket value from the string, treating the string as
untrusted input.
}

@defidform[#:kind "syntax-class" non-empty-string]{
A syntax class for matching non-empty strings in macros.
}
