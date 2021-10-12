#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Introduction}

@margin-note{@other-doc[denxi-white-paper] covers the thinking behind
Denxi.}

Denxi is a model for software distribution. Package management, CI/CD,
and content management are possible use cases for Denxi.  It is
flexible enough to work as a library, as a framework, or as a
standalone application.

Denxi's API has hundreds of functions, so this guide will briefly
cover fundamental concepts. You will learn what a package definition
is, what a launcher is, and a command that will show you where to find
working examples of both in Denxi's source code.

By reading the examples and writing your own package definitions and
launchers, you will come to understand Denxi's reference material
and appreciate a few benefits.

@itemlist[
@item{Your explicit, informed consent is considered sacred.}
@item{Denxi can manage dependencies for projects written in any language, or your whole operating system.}
@item{Denxi defaults to a zero-trust configuration, which avoids @hyperlink["http://www.ranum.com/security/computer_security/editorials/dumb/"]{many problems}.}
@item{If you don't like how a software installation affects your system, you can override it.}
@item{If you don't like Denxi itself, you can use it to create and distribute an alternative.}
@item{(@litchar{npm} users) You'll have more safety checks to protect your system.}
@item{(@litchar{raco pkg} users) You won't mutate your Racket installation when installing software.}
@item{(PLaneT users) You can install multiple versions of a project without generating non-@racket[eq?] bindings.}
@item{(Guix/Nix users) You get the benefits you expect on Windows, macOS, and GNU/Linux distributions out of the box.}
@item{(End users on Steam, Play Store, etc.) A programmer you trust
can use Denxi to transition you a better platform.}
]
