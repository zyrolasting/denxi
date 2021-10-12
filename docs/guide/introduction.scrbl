#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Introduction}

@margin-note{@other-doc[denxi-white-paper] covers the thinking behind
Denxi.}

Denxi is a programming model for distributing data. It reduces the
cost of producing package managers, storefronts, operating systems,
and CI/CD systems. Denxi can work as a library, as a framework, or as
a standalone application.

@other-doc[denxi-reference] covers hundreds of functions. This guide
briefly introduces core concepts and directs you to example
programs. Use them to understand @other-doc[denxi-reference].

By doing this you'll gain a few benefits.


@itemlist[
@item{Denxi practices zero-trust principles to avoid
@hyperlink["http://www.ranum.com/security/computer_security/editorials/dumb/"]{many
problems}. Denxi only performs side-effects that it can trace back to
your explicit, informed consent.}
@item{Denxi can manage dependencies for projects written in any language, or your whole operating system.}
@item{If you don't like how a software installation affects your system, you can override it.}
@item{If you don't like Denxi itself, you can use it to create and distribute an alternative. It creates package managers like Racket creates languages.}
@item{(@litchar{npm} users) You'll have more safety checks to protect your system.}
@item{(@litchar{raco pkg} users) You won't mutate your Racket installation when installing software.}
@item{(PLaneT users) You can install multiple versions of a project without generating non-@racket[eq?] bindings.}
@item{(Guix/Nix users) You get the benefits you expect on Windows, macOS, and GNU/Linux distributions out of the box.}
]
