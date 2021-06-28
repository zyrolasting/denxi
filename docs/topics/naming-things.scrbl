#lang scribble/manual

@title{Binding Cryptographic Hash Functions}

Use @racket[current-chfs] to bind accepted names of cryptographic hash
functions to a trusted implementation.

@racketblock[
(code:comment "'sha224 is the canonical symbol.")
(code:comment "The pattern allows for case-insensivity and optional dashes.")
(code:comment "The implementation lookup specifies the SHA-3 suite.")
(current-chfs `(([sha224 #px"^(?i:sha-?224)$"]
                 ,(chf-find-implementation 'sha3-224))))
]

To understand why this is important, look at these integrity
claims. They use different symbols to represent use of SHA-224.

@racketblock[
(integrity 'sha224 #"...")
(integrity 'SHA-224 #"...")
(integrity 'Sha224 #"...")
]

You might assume these claims mean the same thing, but SHA-224 is
defined in cryptographic @italic{suites} like SHA-2 and SHA-3. The
claims say nothing about which suite we want to use. That, and if a
security incident harms confidence in a particular implementation of
SHA-224, we may need the ability to adapt the implementation without
changing our code.

@racket[current-chfs] solves these problems.
