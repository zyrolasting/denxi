#lang info

(define provider "sagegerard.com")
(define package "zcpkg")
(define edition "draft")
(define revision-number 0)
(define revision-names '("begin-alpha"))
(define setup-module #f)
(define dependencies '())
(define launchers
  '(#hash((name . "zcpkg")
          (gracket? . #f)
          (args . ()))))
