#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Introduction}

@margin-note{@other-doc[xiden-white-paper] covers the thinking behind
Xiden.}
Xiden is a zero-trust, Guix-like programming model for software
distribution. That means Xiden approaches software distribution as a
whole, such that package management, CI/CD, and content management are
use cases. You provide configuration and code to shape Xiden to your
particular project, which does not have to be implemented in Racket.

You'll want to use Xiden if you are a developer who wants more control
over how software arrives on a system, but don't want to spend the
time it takes to solve that problem well. End-users can also benefit
from at least knowing about Xiden because they can have their own say
over how Xiden-powered projects operate on their system. In that
sense, Xiden as a model gives developers and users more options to
freely associate and share work.

These benefits of using Xiden affect developers and end-users.
Developers would realize the benefits first, but are better equipped
to share them with their users.

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

@item{(End users on Steam, Play Store, etc.) Someone can use Xiden to help transition you a new platform.}

]
