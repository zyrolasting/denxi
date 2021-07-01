You may override package inputs for some launcher. One way is to use
the command line interface.

```
racket launcher.rkt \
  +a defn.rkt \
  +o '#px""' '(input "data" (artifact (byte-source #"override") #f #f))'
```

This command line adds overrides to `XIDEN_INPUT_OVERRIDES`, which
applies to all packages in the scope of a transaction.  Each override
takes two arguments. The first is a readable regular expression that
matches against a package query. The second is the code for an input
expression.

The example uses an empty pattern to match all package definitions as
eligible for overriding. We then replace any inputs in those
definitions with the same name as the input expression we provided.

The override applies to all eligible packages, and all inputs of the
same name. This allows you to standardize dependencies in the event
you end up with something like multiple slightly different copies of
Ruby.

```
RUBY='(input "ruby" ...)' racket launcher.rkt do \
  +a ... \
  +o 'syntax-highlighting' "$RUBY" \
  +o 'images' "$RUBY"
```
