#lang xiden/launcher

#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
       <skull initialled "jgs", https://ascii.co.uk/art/skulls>

                                .-.
                               (0.0)
                             '=.|m|.='
                             .='`"``=.

       /!\ DANGER /!\ - MAXIMUM TRUST LAUNCHER - /!\ DANGER /!\

  This launcher allows every dangerous operation so that you can see
  Xiden's attack surface.

  High trust makes Xiden's built-in CLI convenient, at the cost of giving
  package definitons more power. Package definitions are a publishable
  format that controls I/O operations on a host, so they must be treated
  with the same regard as shell scripts.

  Even with zero-trust, be sure you check your operating system
  permissions for a Xiden process no matter what. Running this
  file as an administrator on an untrusted package definition is
  like running `curl -k ... | sudo sh`.

  This extreme exists as a consequence of Xiden's deeply-configurable
  model. To understand what "deeply" means, look how little code there
  is here. The gap from zero to maximum trust is not small, but trusting
  _just the right things_ can defeat entire subsystems. Take
  `XIDEN_TRUST_BAD_DIGEST`, which we've used for many examples. That
  trust defeats all data verification, effectively implying trust for
  all settings in `XIDEN_TRUST_*`! That one boolean creates a path to
  arbitrary code execution when paired with a malicious package
  definition.

  Why allow the risk? Because it lets us talk about security when we're
  ready, and it's relevant. When we're just dealing with a stupid little
  string in a package definition, no network access is necessary for an
  installation. Trusting toy data makes it a lot easier to focus on what
  the example is about. Subtle changes in user experience like that make
  these scary settings worth having around.

||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

; Run any executable that a package definition wants to run.
(XIDEN_TRUST_ANY_EXECUTABLE #t)

; Trust any content
(XIDEN_TRUST_BAD_DIGEST #t)

; I'm okay with HTTP, and bad certificates over HTTPS.
(XIDEN_TRUST_UNVERIFIED_HOST #t)

; Push my computer as hard as you want.
(XIDEN_MEMORY_LIMIT_MB +inf.0)
(XIDEN_TIME_LIMIT_S +inf.0)

; Download without limits on space or time.
(XIDEN_FETCH_TOTAL_SIZE_MB +inf.0)
(XIDEN_FETCH_TIMEOUT_MS +inf.0)

; Run as `root` if you love spam and want lots of it.
(module+ main (launch-xiden!))
