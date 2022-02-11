#lang reader "../scribble.rkt"

@title{22 January 2022}
@by-slg

In reviewing my code with the power of hindsight, I found a lot I
could delete. I do so mercilessly. As of my local branch, workspaces
and package queries have been removed for being more than they needed
to be.

I don't consider my earlier work to be the product of bad judgement,
but I do see elements of mistakes I was hoping to avoid. When I
removed package queries, I had to remove defined taxa. This in turn
destroyed the state model. By allowing for user-defined identities, a
lot of code in Denxi became unecessary. Workspaces and queries were an
artifact of premature abstraction, where I decided filesystems and
SQLite were a necessary mechanism for state. This does not reflect
reality, although it did make Denxi immediately useful. That has never
been a goal. In my experience, immediate usefulness is antithetical to
future-proofing. I made Denxi so that all subjective elements of
sharing work must be deferred as late as possible. I now see a path
where Denxi has no effect except in memory. Unit tests would no longer
generate I/O traffic, or accidentally leave temporary files behind.

Without package queries or workspaces, it no longer made sense to give
packages an identity. When users can dynamically customize packages,
it only really makes sense to provide an identity to a package
output. Nothing effectual happens in Denxi until it processes an
output, and I defer use of most features until that moment. This
promotes a view that is friendly to lazy evaluation and paralell
processing, but I wouldn't yet call it condusive to that purpose. The
state model is shrinking, because Denxi uses the semantics of an
environment with bound identifiers, as it applies to user-controlled
storage. When you bound a value like an output in Denxi before the
current refactoring, the binding took the form of a symbolic link
acting as a reference to a target file or directory. This is not a
binding according to pedants, but it is a notion of a binding that the
user can protect. That's good enough. So long as outputs are bound,
all other discovery information can be framed in terms of outputs.

The package struct collapsed to metadata, input, and output. I didn't
see reason to preserve @tt[denxi/racket-versions] or the OS support
fields when they could be easily replaced by per-output assertions
like so:

@racketblock[
(output "default"
  (require-os windows)
  ...)
]

The metadata is tricky when it is user-defined, because they exist as
what Owen Barfield would call a logomorphism. Metadata is subjective,
yet prescriptive of a logic understood by the package author. The act
of prescribing a logic means possibly contradicting or dismissing
another, namely the logic of Denxi's user. I see
@racketid[current-package-editor] as a method of canonicalization in
the user's favor when dealing with such messiness.
