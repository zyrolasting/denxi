First start `server.rkt` in the background.  For any HTTP request, the
server has a 1 in 5 of responding with data in a 200 OK response.

If you try to install this definition directly, it will fail until it
won't. Once it works, pipe the content of the installed file to
`lock.rkt`'s standard input. This will print a locked artifact datum
with (demo) integrity and signature information.

The generated artifact is useful for lock files and reproducible
builds.

Bonus: The output artifact has integrity and signature information.
Edit the launcher's configuration to leverage that.
