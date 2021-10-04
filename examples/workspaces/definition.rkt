#lang denxi

(input "hello.txt"
  (artifact (text-source "Hello, world!") #f #f))

(output "default"
  (keep-input "hello.txt"))
