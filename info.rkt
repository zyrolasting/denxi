#lang info

(define collection "zcpkg")
(define description "An alternative package manager for Racket, built from lessons learned from PLaneT and raco pkg.")
(define deps '("base" "place-controller" "idiocket"))
(define raco-commands '(("zcpkg" (submod zcpkg/client/main main) "Manage zero-collection packages" #f)))
(define scribblings '(("scribblings/zcpkg.scrbl" ())))
(define version "0.0")
