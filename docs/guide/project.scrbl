#lang scribble/manual

@require["../shared.rkt"]

@title[#:tag "project"]{Project Information}

@hyperlink["https://github.com/zyrolasting/denxi"]{GitHub} hosts the source
code for Denxi. @bold{It is not yet ready for production use.}
Dependency management is a hard problem that takes time to handle well.


@section{Contributing}

To contribute to Denxi, you can do at least one of the following.

@itemlist[
@item{@hyperlink["https://www.paypal.com/paypalme/sagegerard"]{Contribute funds to support development}}
@item{Open a pull request against the main repository.}
@item{@hyperlink["https://github.com/zyrolasting/denxi/issues"]{Open an issue}}
]


@subsection{A Note For Programmers}

The tools in use are Racket, SQLite3, OpenSSL, Make, and C. I list them in the
order of prominence, so expertise in an earlier item will help you understand a
larger volume of source code.

While there are no style rules in place, please make your code visually similar
to surrounding code. Please take care to produce lean diffs.


@subsection{High-value Contributions}

@margin-note{Denxi's Racket source would ideally know nothing about the underlying platform.}
@itemlist[

@item{
@bold{Windows support}.
This will entail some knowledge of C, MSVC, and the Windows API for the best end-user experience.
}

@item{
@bold{Security audits}.
If you are trained in information security, pointing out vulnerabilities is greatly appreciated.
}

@item{
@bold{Translations}.
Denxi is designed to load user-facing strings lazily given locale information, make it relatively easy to offer a translation to a different human language.
}

]
