#lang racket/base

(provide (all-defined-out))
(require "message.rkt")

(define-message $start (workspace-dir config))
(define-message $sentinel ())
(define-message $fail (to-display))
(define-message $output (v))
(define-message $stop ())
(define-message $before-making-orphans (dependents dependency))
(define-message $already-installed (info))
(define-message $on-compilation-error (message))
(define-message $on-bad-digest (info))
(define-message $on-bad-signature (info))
(define-message $on-missing-signature (info))
(define-message $on-unverified-host (host))
(define-message $on-package-installed (info))
(define-message $install-package (infos url-or-path))
