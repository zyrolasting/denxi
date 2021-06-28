#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Why Xiden?}

@margin-note{@other-doc[xiden-white-paper] covers the thinking behind
Xiden.}
Xiden is an application and a library for distributing software.  It
is one of many projects to make that claim, so you'll want to use it
for these benefits.

@itemlist[

@item{Your explicit, informed consent is considered sacred because of
a zero-trust configuration (The alternative is
@hyperlink["http://www.ranum.com/security/computer_security/editorials/dumb/"]{a
really bad idea)}.}

@item{If you don't like how a software installation affects your system, you can override it.}

@item{If you don't like Xiden itself, it will help you create an alternative that fits you.}

@item{(@litchar{npm} users) You'll have more safety checks to protect your system.}

@item{(@litchar{raco pkg} users) You won't mutate your Racket installation when installing software.}

@item{(PLaneT users) You can install multiple versions of a project without generating non-@racket[eq?] bindings.}

@item{(Guix/Nix users) You get the benefits you expect on Windows, macOS, and GNU/Linux distributions out of the box.}

@item{(End users on Steam, Play Store, etc.) Someone can use Xiden to help you get out from under corporate interests.}

]
