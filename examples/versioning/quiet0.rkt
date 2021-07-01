#lang xiden
(provider "example.com")
(name "greeting")
(edition "quiet")
(revision-number 0)
(revision-names "initial" "oldest")
(input "hello.txt" (artifact (text-source "hello, world") #f #f))
(output "default" (keep-input "hello.txt"))
