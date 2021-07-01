Blank output terms like `(output "default")` are only really good for
inducing cache hits if you lose access to a link.

  racket launcher.rkt do +a install-first.rkt
  rm default
  racket launcher.rkt do +a install-second.rkt

You can also induce the collision in one transaction, without deleting
the link.

  racket launcher.rkt do \
         +a install-first.rkt \
         +d cached install-second.rkt
