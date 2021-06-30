This example installs an HTML and CSS files embedded directly in the
package definition, showing that project assets may appear
inline. Selecting a different output results in a different page
appearance, despite using one reference to the same HTML file.

1. Run `racket launch.rkt do +s dark-mode dark defn.rkt`. You will see
   a symlink named `dark-mode` appear. Inside is an HTML file and a
   CSS file.  The HTML page appears black.

2. Run `racket launch.rkt do +s light-mode light defn.rkt`. You will a
   symlink named `light-mode` appear. Inside is the a link to the
   _exact same_ HTML file, but no CSS file with it. The HTML page
   appears white.

**Note:** If you open the index.html link with your browser, you may
see HTML _source code_, not a displayed page. If this happens, just
copy the original files to a different directory with the same names
as the links.
