#lang racket/base

; Define a derivation and the operations used to create
; unique and internally-consistent directories on disk.

(require "contract.rkt")

(provide make-build-sandbox
         make-derivation-module)

(require racket/function
         racket/path
         racket/runtime-path
         "config.rkt"
         "exn.rkt"
         "file.rkt"
         "localstate.rkt"
         "message.rkt"
         "path.rkt"
         "rc.rkt"
         "sandbox.rkt"
         "setting.rkt"
         "source.rkt"
         "workspace.rkt")

(define+provide-message $consent-note ())
(define+provide-message $no-package-info (source))

(define-runtime-path module-language-path "derivation-forms.rkt")

(define (make-derivation-module inputs outputs)
  `(module derivation ,module-language-path
     (define input-ref
       (let ([h ,inputs])
         (λ (key)
           (fetch-input (hash-ref h key (λ () (error "No such input: ~a" key)))))))
     (define build!
       (let ([h ,outputs])
         (λ (key)
           (eval (hash-ref h key (λ () (error "No such output: ~a" key)))))))))


(define (build-derivation input-program directory outputs)
  (define s (make-build-sandbox input-program directory))
  (for ([output-name (in-list outputs)])
    (s `(build! ,output-name))))


(module+ test
  (require rackunit
           "file.rkt")

  (with-temporary-directory
    (build-derivation (make-derivation-module
                       (hash "other-lib" "foo.so")
                       (hash "doc" "built-doc"
                             "lib" "built-lib"
                             "tst" "built-tst"))
                      (current-directory)
                      '("doc" "lib"))))
