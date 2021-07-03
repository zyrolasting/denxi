#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Introduction}

@margin-note{@other-doc[xiden-white-paper] covers the thinking behind
Xiden.}
You can think of Xiden as a zero-trust, Guix-like package manager. But
in truth, Xiden is a programming model for software
distribution. Package management, CI/CD, and content management are
possible use cases. You provide configuration and code to shape Xiden
to your particular project, which does not have to be implemented in
Racket.

Use Xiden if you are a developer who wants to control how software
arrives on a system, quickly. End-users benefit from at least knowing
about Xiden because they can have their own say over how Xiden-powered
projects operate on their system. In that sense, Xiden as a model
gives developers and users more options to freely associate and share
work.

These benefits of using Xiden affect developers and end-users.
Developers would realize the benefits first, but they are equipped to
pass them along to their users.

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
