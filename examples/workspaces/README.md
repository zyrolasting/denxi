A workspace is a directory that Xiden uses to hold state.
Details: `raco doc workspace L:xiden/state`

You can see the path to your default workspace directory by running
the below command.

```
  $ racket -l racket/base -l xiden/state -e '(XIDEN_WORKSPACE)'
```

This example has two launchers. Each pick their workspace locations
differently. Use them to install something, and look inside the new
directory. You'll find content-addressable files and links in an
`objects` directory, and a SQLite3 database called `db`. The database
is left in sync with `objects` unless either is directly modified.

Protect your workspaces! Xiden trusts its state, which makes
workspaces part of the attack surface. Limit workspace access to the
user who owns it, and limit the write permissions of that user. If
Xiden is compromised via state, it can be programmed to delete files
of an attacker's choice.
