#lang denxi/document

@title{Canonicalization}

@defmodule[denxi/canon]

@defmodule[denxi/canon] allows a user to assign canonical names to
other people's names.  Since we disagree on names for all logical
entities, we need canonicalization as a form of prescriptivism.

@defproc[(logomorphism [prescription string?] [experience any/c] [utterance string?]) (or/c #f string?)]{
Returns @racket[#f], or @racket[prescription]. Returns
@racket[prescription] when one of the following conditions is true.
Conditions are checked in the order shown.

@itemlist[#:style 'ordered
@item{Exact string match: @racket[(equal? experience utterance)]}
@item{Exact string match, among a list of options: @racket[(member utterance experience)]}
@item{Pattern match by regular expression: @racket[(regexp-match? experience utterance)]}
@item{Match by predicate: @racket[(experience utterance)]}
]


The semantics of this model comes from a circular use of Owen
Barfield's “@deftech{logomorphism}.”  To paraphrase, a logomorphism is
a term created by our tendency to name experiences as if the logical
content of the name is sufficient for capturing meaning. The model is
circular, because @racket[logomorphism] is a logomorphism about
logomorphisms. This strange loop concisely captures gives subjective
names an identity in Denxi's model. Discovery information like
versions, names, or tags are inherently meaningless to a user, unless
the user chooses to adopt's another scheme as their own. Logomorphisms
make this commitment explicit, so that they can be modified
independently of a standard.
}

@defform[(canon [prescription experience] ...)]{
Expands to a @deftech{canon}. A canon is a unary function that applies
a disjunction of logomorphisms to a string argument.

@racketblock[
(lambda (utterance) (or (logomorphism prescription experience utterance) ...))
]

The cardinality of the canon's range is one plus the number of
possible strings returned by the canon. Canons and logomorphisms
together capture structured and empirical naming patterns.  For
example, a taxonomy is a canon for taxa. A taxon is a logomorphism
with its own canonical elements.

@racketblock[
(define (taxonomy [classification . rules] ...)
  (canon [classification (canon . rules)]
   	 ...))   
]
}
