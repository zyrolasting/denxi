# Setup

This section covers how to install `xiden` on your system.

## 1. Required Programs

First, make sure the following programs are available in your `PATH`.

* SQLite 3.24.0+. Verify with `sqlite3 -version`

* Racket 7.0+. Verify with `racket -v`

* OpenSSL 0.9.8+. Verify with `openssl version`

## 2. Step 1: Get the Code

There are currently no pre-built binaries for Xiden, so we need to build
from source. The goal of this step is to get a directory named `xiden`
somewhere on your disk.

You can either use Git or download an archive.

### 2.1. Method: Use Git

If you wish to use Git, you can clone the default branch from Xiden’s
repository. If you use SSH, then run `git clone
git@github.com:zyrolasting/xiden.git`.  Or, if you use HTTPS, then run
`git clone https://github.com/zyrolasting/xiden.git`

Either command will leave a `xiden` directory in your working directory,
and you can move on to Step 2.

### 2.2. Method: Download Archive

If you do not have Git, then you can download a ZIP file of the latest
source code from the following link:

[https://github.com/zyrolasting/xiden/archive/master.zip](https://github.com/zyrolasting/xiden/archive/master.zip)

You can then extract the archive using whatever tool you prefer.

## 3. Step 2: Build The Project

Enter the source directory created from Step 1. We will now build the
executable and offline documentation. If you see `Makefile` in your
current directory, then you are in the right place.

If you have GNU Make installed, then just run `make`. Otherwise, open
`Makefile` and run the commands defined by the `build` target.

If successful, then `xiden` should now work as a command in your shell.
If not, check your `PATH` to make sure that it includes the directory
where `raco setup` creates launchers on your disk.

Alternatively, you can access the same command line interface using the
`raco zcpkg` (“zero collection package”) command.
