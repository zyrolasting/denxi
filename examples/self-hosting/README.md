This example installs another Xiden in an isolated copy of minimal
Racket 7.9 CS. This example is currently designed for Unix-like
systems.

**Security notice**: The rcfile trusts the author's public key, the
host system's `sh`, and the exact `racket` executable produced by the
Racket installation. This makes the transaction vulnerable to a
compromised shell. Note that the Racket installation script is signed
by the author of this example, _not_ an official distributor (Racket
does not seem to include signature at time of writing). Base your
actual trust in this example on the trust in your host system, and in
Sage Gerard's public key.

To install, run `racket launch do +a defn.rkt`. You will see a symlink
appear.  The link points to a directory that holds a fresh Racket
installation.

Another Xiden instance will be inside the Racket installation. New
Xiden instances can be used to run this example ad infinitum, provided
that the installed version of Xiden is functionally-equivalent to the
version used to run this example (it might not be, since the Xiden
instance is sourced from the default Racket package catalog). This
example can be adjusted to get an exact Xiden version if desired.
