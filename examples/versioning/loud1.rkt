#lang xiden
(provider "example.com")
(name "greeting")
(edition "loud")
(revision-number 1)
(revision-names "no-exclaim")
(input "hello.txt" (artifact (text-source "HELLO, WORLD.") #f #f))
(output "default" (keep-input "hello.txt"))
