#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base racket/file]]

@title[#:tag "inputs"]{Inputs}

How do you reliably reproduce the same software? One way is to check
all of your dependencies and build environment information into source
control. If you are doing this and having success, then you are
probably not reading this. The other approach is to effectively place
an order for the same data, as if they were sandwiches served by a
deli.

In that sense, an @deftech{input} is like an order for a sandwich.
Except instead of getting a sandwich, you get bytes.  Unless forced to
do otherwise, @|binary| will only build a package if every bit it
fetches from an outside source is to order.

A package definition can declare arbitrary files or other packages as
inputs. This way you can fetch other packages, or even individual
libraries that are not packaged for use in @|binary|. @|binary| will
check several sources for each input, and will raise an error if it
cannot produce a bit-for-bit copy of all requested data.

Each @tech{input} can take one of several forms:

@itemlist[
@item{A @tech{package query}}
@item{An associative list}
@item{A path to a @racketmodname[setup/infotab] module}
@item{A @racketmodname[setup/infotab] module}
]


@racketblock[
(define inputs
  '("other.rkt"
    [(kind file)
     (name "recipe.txt")
     (sources catalogs
              "https://example.com:99/files"
              "./file.txt")
     (integrity (b32 ""))
     (signature (b32 ""))]
     [(kind )]))]
