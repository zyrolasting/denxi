First start `server.rkt` in the background.  For any HTTP request, the
server has a 1 in 5 chance of responding with `200 OK`.

Keep trying to install this definition until it works. Then, pipe the
content of the installed file to `lock.rkt`'s standard input. This
will print a locked artifact datum with (demo) integrity and signature
information.

The generated artifact is useful for lock files and reproducible
builds.

The output artifact has integrity and signature information. As an
added exercise, edit the launcher's configuration to leverage that.
