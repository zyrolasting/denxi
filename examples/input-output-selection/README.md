This example installs HTML and CSS files embedded directly in the
package definition. Selecting an output impacts the page's appearance
despite using one reference to the same HTML file.

1. Run `racket launch.rkt do +s dark-mode dark defn.rkt`. You will see
   a symlink named `dark-mode` appear. The HTML page inside appears
   black (If your browser has trouble with the symbolic link, just
   copy the HTML file somewhere else).

2. Run `racket launch.rkt do +s light-mode light defn.rkt`. You will a
   symlink named `light-mode` appear. Inside is the a link to the
   _exact same_ HTML file, but no CSS file with it. The HTML page
   appears white.

When I say _exact same_, I mean that the page is a symbolic link to
the same `index.html` file in Denxi's state, because that's the only
HTML file we defined. This is how inputs are cached.
