`certify` creates digest and signature files in terms of Denxi's
cryptographic backend. `verify` checks those files against content.

You can use Denxi's intentionally-leaked RSA keypair to practice.  Run
`racket -l denxi/signature/snake-oil` in this directory to create a
copy of the leaked private key, leaked password, and public key.

Now let's create arbitrary files.

```
echo 'some data' > some.junk
echo 'more data' > more.junk
```

You should now have two `.junk` files, one `.txt` file, and two `.pem`
files.

Run this command to create digests and signatures for the `.junk`
files.

```
./certify sha3-512 \
          LEAKED-private-key.pem \
          LEAKED-private-key-password.txt \
	  *.junk
```

You can then verify the integrity and signature information.  Run this
command to see checkmarks indicating which check passed.

```
./verify sha3-512 public-key.pem *.junk
```

**Exercise:** Change the contents of the junk files.  How does this
impact the output of the `verify` command?

**Exercise:** How might you adapt this example to output signatures
only, such as `.asc` files for GPG users?

**Exercise:** Create a directory for use with
`make-filesystem-shovel`.  Use `certify` to populate that directory,
such that `(input "story" "story.txt")` works in a package definition.
