This example has three definitions that each represent a release of
the same product. The editions indicate the design used for the
software, and the revisions indicate implementations for that design
made over time. See `raco docs xiden/version` for definitions.

Each package definition includes its own version information, but they
share the name `greeting`. If we abbreviate our installation request
using `do +a` or `do ++install-abbreviated`, then Xiden will pick as
`greeting` as the name of each version's link.  We avoid this conflict
by building a transaction that uses the default output, but allows us
to specify our own link names.

```
racket launch.rkt do \
       +d loud0 loud0.rkt \
       +d loud1 loud1.rkt \
       +d quiet quiet0.rkt
```

Once that's done, take this oppurtunity to run `xiden show installed`.
The output will look something like this.

```
example.com:greeting:loud:0:0:ii  default yk2bp2qbwrdgtb9gwacv8tdqrjakyp2d
example.com:greeting:loud:1:1:ii  default y19r2j34yja9g3b8ygzmc4rmjspkj85h
example.com:greeting:quiet:0:0:ii default yja5p81pjtdsqaztrkhh0b1n9sjgb1vj
```

The first column is an _exact package query_ that identifies the
package definition used at the time (`raco docs xiden/query`). The
second column is the name of the output used in that definition. The
third column is the name of the directory in the workspace's `objects`
directory representing the installed output.
