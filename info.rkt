#lang info

(define collection "zcpkg")
(define description "A better package manager for Racket")
(define deps '("base" "web-server-lib"))
(define raco-commands '(("zcpkg" (submod zcpkg/client/main main) "Manage zero-collection packages" #f)))
(define scribblings '(("scribblings/zcpkg.scrbl" ())))
(define version "0.0")
