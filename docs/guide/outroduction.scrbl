#lang scribble/manual

@require[@for-label[racket/base
                    xiden/source]
         "../shared.rkt"]

@title[#:tag "guide-end"]{Now What?}

You installed Xiden, wrote a package definition, installed software
using your own launcher, then uninstalled that software.  You now know
the basics of Xiden, but this guide only shows you the entrance of a
rabbit hole.

We never discussed @tech/xiden-topics{package conflicts}, dependency
hell, or the other tricky problems in software distribution. I hope
you will be motivated to continue once you realize that you can
distribute @litchar{my-xiden.rkt} using @litchar{xiden}. The
implications of that fact can be very liberating.

@margin-note{Optional: read @other-doc[xiden-white-paper] before
@other-doc[xiden-topics] to better understand design decisions and
broad project direction.}

You can now read @other-doc[xiden-topics], which is full of
self-contained tutorials and notes. The sections do not flow together,
but they don't have to. You only need to remember that everything
boils down to package definitions and launchers.

Start from where this guide cited sections of
@other-doc[xiden-topics]. Explore from there until you find yourself
reading @other-doc[xiden-reference]. Once you understand the
reference, you will write powerful launchers that not only distributes
your software, but helps you organize communities.

Thank you for reading this guide, and for trying Xiden. My name is
Sage, and if you have any questions or comments, please email me at
the address found on @hyperlink["https://sagegerard.com"]{my page}. I
can also be found on
@hyperlink["https://discordapp.com/invite/6Zq8sH5"]{Racket's Discord}.
