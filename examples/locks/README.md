This example uses a pre-built workspace as a lock for reproducible
builds. Run `racket launch.rkt do +a defn.rkt`. You will see a symlink
appear with exact content despite the package definition itself
containing no useful information.
