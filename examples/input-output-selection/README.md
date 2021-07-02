This example installs an HTML and CSS files embedded directly in the
package definition, showing that project assets may appear inline, and
selecting an output means selecting a new deliverable. Here, an output
results in a specific page appearance despite using one reference to
the same HTML file.

1. Run `racket launch.rkt do +s dark-mode dark defn.rkt`. You will see
   a symlink named `dark-mode` appear. The HTML page inside appears
   black (Aside: If your browser has trouble with the symbolic link,
   just copy the HTML file to a normal empty directory).

2. Run `racket launch.rkt do +s light-mode light defn.rkt`. You will a
   symlink named `light-mode` appear. Inside is the a link to the
   _exact same_ HTML file, but no CSS file with it. The HTML page
   appears white.

When I say _exact same_, I mean that the page is a symbolic link to
the same `index.html` file in Xiden's state, because that's the only
HTML file we defined. This is how inputs are cached.
