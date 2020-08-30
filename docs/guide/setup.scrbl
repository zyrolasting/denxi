#lang scribble/manual

@title{Setup}

These instructions assume that you've checked out the
@hyperlink["https://github.com/zyrolasting/xiden"]{source code}.

First, make sure the following programs are available in your search
paths.

@itemlist[
@item{SQLite 3.24.0+. Verify with `sqlite3 -version`}
@item{Racket 7.0+. Verify with `racket -v`}
@item{OpenSSL 0.9.8+. Verify with `openssl version`}
]

Next, build the project using `make` and link the source directory as
a collection in your Racket installation.

@verbatim|{
git clone ...
raco link xiden
cd xiden
make
}|
