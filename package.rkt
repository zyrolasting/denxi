#lang racket/base

(require racket/contract)
(provide (struct-out package)
         (struct-out $package)
         (struct-out $package:output)
         (struct-out $package:output:built)
         (struct-out $package:output:reused)
         (struct-out $package:output:undefined)
         (contract-out
          [current-package-editor
           (parameter/c (-> package? (subprogram/c package?)))]
          [empty-package
           package?]
          [install-package
           (-> mind-implementation/c
               source?
               string?
               string?
               (subprogram/c void?))]))



(require racket/file
         racket/format
         racket/function
         "input.rkt"
         "known.rkt"
         "mind.rkt"
         "message.rkt"
         "monad.rkt"
         "output.rkt"
         "pkgdef/static.rkt"
         "port.rkt"
         "racket-module.rkt"
         "source.rkt"
         "subprogram.rkt")


(module+ test
  (require racket/function
           rackunit
           (submod "subprogram.rkt" test)
           "file.rkt"))


(define-message $package ())
(define-message $package:output $package (name))
(define-message $package:output:built $package:output ())
(define-message $package:output:reused $package:output ())
(define-message $package:output:undefined $package:output (names))


(define current-package-editor
  (make-parameter subprogram-unit))

(struct package
  (metadata outputs))


(define empty-package
  (package (hasheq) null))


(define-syntax-rule (build-package . fields)
  (struct-copy package empty-package . fields))


(define (install-package mind source key rkey)
  (mdo d := (load-package-definition source rkey)
       p := (load-package d)
       i := (load-package-inputs p)
       o := (load-package-output p key)
       (bind-output mind i o rkey)))


(define (load-package-definition source name max-size)
  (mdo variant := (subprogram-fetch name source (make-limited-tap max-size))
       (read-package-definition variant)))


; Use the module namespace to dynamically extend registered modules.
; Depends on package definition safety checks to prevent use of untrusted code.
(define-namespace-anchor anchor)
(define module-namespace (namespace-anchor->namespace anchor))
(define-subprogram (load-package pkgdef)
  (eval pkgdef module-namespace)
  ($use (dynamic-require `',(cadr pkgdef) 'pkg)))


(define-subprogram (load-package-output pkg name)
  (define output
    (findf (compose (curry equal? name)
                    package-output-name)
           (package-outputs pkg)))
  (if output
      ($use output)
      ($fail ($package:output:undefined name
                                        (map package-output-name (package-outputs pkg))))))


(define-subprogram (load-package-inputs pkg)
  (define all-inputs (package-inputs pkg))
  (define input (findf abstract-package-input? all-inputs))
  (if input
      ($fail ($package:input:abstract (package-input-name input)))
      ($use all-inputs)))


(define-subprogram (bind-output mind inputs output key rkey)
  (define (may-cycle messages)
    (parameterize ([current-inputs inputs])
      (run-subprogram (if output
                          ((package-output-make-subprogram output))
                          (subprogram-failure ($package:output:undefined key)))
                      messages)))
  (mdo known := (mind-recall mind key)
       names := (known-get-names known)
       (subprogram-acyclic key may-cycle)
       (known-put-names known (cons rkey names))))


(module+ test
  (let* ([target (package-output "_" null void)]
         [ev (build-package [outputs (list target)])])
    (test-subprogram
     "Allow only defined outputs"
     (load-package-output ev "_")
     (λ (val messages)
       (check-eq? val target)
       (check-pred null? messages)))
    (test-subprogram
     "Reject unavailable outputs"
     (load-package-output ev "whatever")
     (λ (val messages)
       (check-eq? val FAILURE)
       (check-match messages
                    (list ($package:output:undefined
                           "whatever"
                           '("_")))))))

  (let* ([inputs (list (package-input "c1" "s")
                       (package-input "c2" "s"))]
         [ev (build-package [inputs inputs])])
    (test-subprogram
     "Allow all concrete inputs"
     (load-package-inputs ev)
     (λ (val messages)
       (check-eq? val inputs)
       (check-pred null? messages))))

  (test-subprogram
   "Disallow abstract inputs"
   (load-package-inputs
    (build-package [inputs (list (package-input "concrete" "")
                                 (make-package-input "a"))]))
   (λ (val messages)
     (check-eq? val FAILURE)
     (check-match messages
                  (list ($package:input:abstract "a"))))))
