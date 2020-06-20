#lang reprovide
"models/account.rkt"
"models/party.rkt"

#|
  An account:
    Represents at least one party (person or organization)
    Has one generated ID, acting as a password (See Mullvad's onboarding)

  A party:
    Has a common name
    Has an optional public key
    Has at least zero projects

  A brand:
    Represents a marketable offering
    Has summary content (xexpr)
    Has at least zero lines

  A line:
    Represents a progression of versioned artifacts
    Has a summary
    Has at least zero artifacts

  An artifact:
    Represents a published work
    Has a hash, for integrity
    Has an optional signature, for authentication. Corresponds to party public key.
|#
