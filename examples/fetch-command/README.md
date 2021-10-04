The fetch command uses Denxi's content fulfillment features.  Give it
a source as you've seen it in package definitions up until now.

```
denxi fetch '(text-source "Hello, world!")'
```

Data transfer safety limits are active. Downloads like this one may
fail if estimated size of the payload is too high, or not known (which
Denxi takes to mean "unlimited").

```
source='(http-source "https://racket-lang.org/")'
denxi fetch "$source" # may fail
denxi fetch -m '+inf.0' "$source"
```

Warning: Data is dumped directly to standard output. To avoid messing
up your terminal emulator, pipe it somewhere else.

```
denxi fetch '(byte-source "#\1\2\3")' >data
```

You can use this command to download data you see in package
definitions, but it will not verify the output data.
