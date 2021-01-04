#lang scribble/manual

@require["../shared.rkt"]

@title{Configuration}

Xiden dynamically binds configurable values using
@tech{settings} when launched. A @deftech{runtime configuration} is a
@tech/reference{parameterization} in which every @tech{setting}
defined by @racketmodname[xiden/rc] is bound to a value that cannot be
overridden except by another @tech/reference{parameterization}.

@include-section{modules/setting.scrbl}
@include-section{modules/rc.scrbl}
