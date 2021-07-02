#lang xiden

(input "light.css"
       (artifact (text-source "body { background: white }\n")
                 #f
                 #f))

(input "index.html"
       (artifact
        (lines-source
         #f
         '("<DOCTYPE html>"
           "<html>"
           "  <head>"
           "    <style>body { background: black }</style>"
           "    <link rel=\"stylesheet\" href=\"light.css\" />"
           "    <title>Colored by output</title>"
           "  </head>"
           "</html>"))
        #f
        #f))

(output "light"
        (keep-input "index.html")
        (keep-input "light.css"))

; dark = absense of light
(output "dark"
        (keep-input "index.html"))
