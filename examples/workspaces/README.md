A workspace is a directory that contains a persistent state for Denxi.
Run `raco doc workspace L:denxi/state` for more.

When you launch, `DENXI_WORKSPACE` initializes to a default workspace
directory. You can see its path outside of a custom launcher with this
command.

```
  $ racket -l racket/base -l denxi/state -e '(DENXI_WORKSPACE)'
```

This example has two launchers. Each pick their workspace locations
differently. Use each of them to install the provided definition.
Look inside the new workspace directories. You'll find
content-addressable files and links in an `objects` directory, and a
SQLite3 database called `db`. Denxi keeps the database
logically-consistent with the `objects` directory.

**Protect your workspaces!** Workspaces are an input that can
change or break Denxi's behavior, so they are part of the attack
surface. Limit workspace write access to the user who owns it, and
limit the write permissions of that user.
