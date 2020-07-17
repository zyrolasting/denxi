#lang racket/base

; Define relevant async events as messages.

(provide (all-defined-out))
(require "message-pump.rkt")

(define-message $start (id config))
(define-message $schedule (dependent dependencies))
(define-message $frontlog (schedule))
(define-message $backlog  (schedule))
(define-message $crash (id message))
(define-message $done (id message))
(define-message $stop ())
