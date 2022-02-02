#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/string
		    denxi/input
		    denxi/known
		    denxi/message
		    denxi/monad
		    denxi/output
		    denxi/package
		    denxi/pkgdef/static
		    denxi/port
		    denxi/racket-module
		    denxi/setting
		    denxi/source
		    denxi/state
		    denxi/subprogram]
         @for-syntax[denxi/package]
         denxi/package
         "../../shared.rkt"]

@title{Packages}

@defmodule[denxi/package]

@defstruct*[package ([metadata (hash/c symbol? string?)]
                     [inputs (listof package-input?)]
                     [outputs (listof package-output?)])]{
A @deftech{package} is an instance of @racket[package].  Packages
relate inputs to outputs with user-defined metadata, where metadata
keys are compared using @racket[eq?].
}


@defthing[empty-package package?]{
The @tech{package} with no content.
}

@defthing[current-package-editor
          (parameter/c (-> package? (subprogram/c package?)))]{
Like @racket[current-package-definition-editor], but for packages.
}


@section{Package Messages}

@defstruct*[($package $message) () #:prefab]{
A @tech{message} pertaining to a package.
}

@defstruct*[($package:output $package) ([name string?]) #:prefab]{
A @tech{message} pertaining to a package output.
}

@defstruct*[($package:output:built $package:output) () #:prefab]{
A package output was successfully added to state within the context of
a running transaction.
}

@defstruct*[($package:output:reused $package:output) () #:prefab]{
A package output was successfully found and reused within the context of
a running transaction.
}

@defstruct*[($package:output:undefined $package:output) ([available (listof string?)]) #:prefab]{
An output is not defined in a package. @racket[available] shows what
outputs are available.
}
