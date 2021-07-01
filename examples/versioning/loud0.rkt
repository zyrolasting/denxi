#lang xiden
(provider "example.com")
(name "greeting")
(edition "loud")
(revision-number 0)
(revision-names "initial" "oldest")
(input "hello.txt" (artifact (text-source "HELLO, WORLD!!") #f #f))
(output "default" (keep-input "hello.txt"))
