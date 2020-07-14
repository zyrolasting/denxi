#lang racket/base

; Define metadata about a zero-collection package.

(provide (all-defined-out))

(require "contract.rkt"
         "config.rkt"
         "metadata.rkt"
         "string.rkt"
         "workspace.rkt")

(struct zcpkg-info
  (provider-name     ; The name of the package provider
   package-name      ; The name of the package itself
   edition-name      ; The name of a design used in the package
   revision-number   ; The number of a design's implementation.
   revision-names    ; Aliases for the revision-number.
   installer         ; A Racket module responsible for userspace changes.
   dependencies      ; A list of dependency queries.
   integrity         ; A digest used to verify package contents
   signature)        ; A signature used to authenticate the provider
  #:prefab)

(define (zcpkg-non-revision-identity=? info provider package edition)
  (and (equal? (zcpkg-info-provider-name info) provider)
       (equal? (zcpkg-info-package-name info) package)
       (equal? (zcpkg-info-edition-name info) edition)))

(define (zcpkg-compare-versions a b)
  (and (andmap (λ (p) (equal? (p a) (p b)))
               (list zcpkg-info-provider-name
                     zcpkg-info-package-name
                     zcpkg-info-edition-name))
       (- (zcpkg-info-revision-number a)
          (zcpkg-info-revision-number b))))

(define (zcpkg-info->relative-path info)
  (build-path (zcpkg-info-provider-name info)
              (zcpkg-info-package-name info)
              (zcpkg-info-edition-name info)
              (~a (zcpkg-info-revision-number info))))

(define (zcpkg-info->install-path info)
  (build-workspace-path (ZCPKG_INSTALL_RELATIVE_PATH)
                        (zcpkg-info->relative-path info)))

(define (read-zcpkg-info dir)
  (apply zcpkg-info
         (get-metadata dir
                       (list #f name-string? "provider-name")
                       (list #f name-string? "edition-name")
                       (list #f name-string? "revision-number")
                       (list #t name-string? "revision-names")
                       (list #t path-string? "installer")
                       (list #t (listof string?) "dependencies")
                       (list #t bytes? "integrity")
                       (list #t bytes? "signature"))))
