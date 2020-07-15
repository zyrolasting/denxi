#lang racket/base

; Define relevant async events as messages.

(provide (all-defined-out))
(require "message-pump.rkt")

(define-message $assign-id (id))
(define-message $on-error (message))
(define-message $before-making-orphans (dependents dependency))
(define-message $backlog-job (message))
(define-message $add-job (message))
(define-message $resolve-source (source))
(define-message $on-bad-digest (info))
(define-message $on-bad-signature (info))
(define-message $on-missing-signature (info))
(define-message $on-unverified-host (host))
(define-message $on-package-installed (info))
(define-message $on-package-uninstalled (info))
(define-message $on-idle (id))
(define-message $stop ())
(define-message $install-package (info path source))
(define-message $download-package (info catalog-url source))
(define-message $uninstall-package (dependency-string))
