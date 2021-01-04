#lang scribble/manual

@require["../shared.rkt"
         @for-label[racket/base
                    racket/contract
                    xiden/message
                    xiden/integrity
                    xiden/signature]]

@title{Data Verification}

When Xiden reproduces data from a @tech{source}, that data may
contain dangerous code. Since Xiden cannot detect motives, it
concerns itself only with enforcing the user's level of trust in the
data.  Data verification is addressed using an @tech{integrity check}
and a @tech{signature check} under a @tech{runtime
configuration}. Each check is security critical.

@include-section{modules/integrity.scrbl}
@include-section{modules/signature.scrbl}
