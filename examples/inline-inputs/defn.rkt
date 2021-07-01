#lang xiden

(input "dark.css"
       (artifact (text-source "body { background: black }\n")
                 #f
                 #f))

(input "index.html"
       (artifact
        (lines-source
         #f
         '("<DOCTYPE html>"
           "<html>"
           "  <head>"
           "    <link rel=\"stylesheet\" href=\"dark.css\" />"
           "    <title>Colored by output</title>"
           "  </head>"
           "</html>"))
        #f
        #f))

(output "light"
        (keep-input "index.html"))

(output "dark"
        (keep-input "index.html")
        (keep-input "dark.css"))
