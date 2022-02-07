#lang reader "../document.rkt"

@title{10 January 2022}
@by-slg

I've started this development journal to track activity that does not
fit in a comment or a changelog. Note that everything I write is in my
own words, with the expectation that you understand what Denxi is and
what I am trying to do by making it.

I invite @italic{constructive} criticism in terms of what you read
here.  That will help improve plans before any code changes.  I
emphasize “constructive,” because I am not being paid to work on
Denxi. Respect little red hens, get cake.


@section{Denxi to Lose Fixed Taxonomy, Gain Configurable Canon}

Denxi uses fixed taxa: provider, brand, edition, and revision. The
user may freely define the meaning of strings used in each taxon.
This wasn't good enough to keep naming methods flexible because the
taxa was prescribed, and not necessarily consistent with a user's
logic.

This is a large, breaking change that I intend to have finished by 31
March 2021. Users will experience a smaller interface from Denxi, but
will no longer be able to construct package queries. Instead, all
discovery information for packages become user-defined.


@section{Client-side Refactoring of Racket Packages}

After the above refactor, I intend to design a Denxi launcher that
downloads Racket packages and refactors the source code such that
third-party collections are not installation-specific.

The consequence is that users use Racket packages, but completely
escape the Racket package management system in doing so. I'm excited
about this use case because it involves removing one key element of
uncertainty when predicting what a Racket program will do when you run
it.

A collection path that isn't built into your Racket installation can
mean just about anything. Users do thankfully control what collection
paths resolve to, but other Racket programmers decide them
@italic{first}. I can, in theory, download a Racket package that
claims every possible collection path in an installation such that all
future installations encounter conflicts. That and a few other reasons
made me avoid use of @litchar{raco pkg} and third party collections in
my future projects. Hopefully with this launcher, others can do the
same.
