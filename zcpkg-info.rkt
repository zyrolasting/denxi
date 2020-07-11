#lang racket/base

; Define metadata about a zero-collection package.

(provide (all-defined-out))

(require racket/format
         "workspace.rkt"
         "config.rkt"
         "metadata.rkt")

(struct zcpkg-info
  (provider-name     ; The name of the package provider
   package-name      ; The name of the package itself
   edition-name      ; The name of a design used in the package
   revision-number   ; The number of a design's implementation.
   revision-names    ; Aliases for the revision-number.
   installer         ; A Racket module responsible for userspace changes.
   dependencies      ; A list of dependency queries.
   integrity         ; A digest used to verify package contents
   signature         ; A signature used to authenticate the provider
   upload-timestamp) ; A timestamp marking when the package was accepted by a server.
  #:prefab)

(declare-info-i/o zcpkg-info)

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
