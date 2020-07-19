#lang racket/base

; Define metadata about a zero-collection package.

(provide (all-defined-out))

(require racket/file
         (only-in racket/list drop-right)
         "contract.rkt"
         "config.rkt"
         "string.rkt"
         "workspace.rkt"
         "zcpkg-settings.rkt")

(struct zcpkg-info
  (provider-name     ; The name of the package provider
   package-name      ; The name of the package itself
   edition-name      ; The name of a design used in the package
   revision-number   ; The number of a design's implementation.
   revision-names    ; Aliases for the revision-number.
   setup-module      ; A Racket module responsible for userspace changes.
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

(define (zcpkg-info->relative-path info #:abbrev [remove-num 0])
  (if (>= remove-num 3)
      (build-path (zcpkg-info-package-name info))
      (apply build-path
             (drop-right
              (list (zcpkg-info-provider-name info)
                    (zcpkg-info-package-name info)
                    (zcpkg-info-edition-name info)
                    (~a (zcpkg-info-revision-number info)))
              (min 3 (max 0 remove-num))))))

(define (zcpkg-info->install-path info)
  (build-workspace-path (ZCPKG_INSTALL_RELATIVE_PATH)
                        (zcpkg-info->relative-path info)))

(define (read-zcpkg-info in)
  (define lookup (load-config in))
  (zcpkg-info (lookup 'provider name-string?)
              (lookup 'package name-string?)
              (lookup 'edition name-string?)
              (lookup 'revision-number exact-nonnegative-integer?)
              (lookup 'revision-names (listof name-string?) null)
              (lookup 'setup-module (or/c #f path-string?) #f)
              (lookup 'dependencies (listof string?) null)
              (lookup 'integrity (or/c #f bytes?) #f)
              (lookup 'signature (or/c #f bytes?) #f)))

(define (read-zcpkg-info-from-directory dir)
  (read-zcpkg-info (build-path dir CONVENTIONAL_PACKAGE_INFO_FILE_NAME)))

(define (zcpkg-info->hash info)
  (hash 'provider (zcpkg-info-provider-name info)
        'package (zcpkg-info-package-name info)
        'edition (zcpkg-info-edition-name info)
        'revision-number (zcpkg-info-revision-number info)
        'revision-names (zcpkg-info-revision-names info)
        'setup-module (zcpkg-info-setup-module info)
        'dependencies (zcpkg-info-dependencies info)
        'integrity (zcpkg-info-integrity info)
        'signature (zcpkg-info-signature info)))

(define (write-zcpkg-info-to-directory info dir)
  (make-directory* dir)
  (call-with-output-file #:exists 'truncate/replace
    (build-path dir CONVENTIONAL_PACKAGE_INFO_FILE_NAME)
    (λ (o) (write-zcpkg-info info o))))

(define (write-zcpkg-info info o)
  (save-config!
   (make-config-closure
    (zcpkg-info->hash info)
    '(provider
      package
      edition
      revision-number
      revision-names
      setup-module
      dependencies
      integrity
      signature))
   o))

(define-syntax-rule (copy-zcpkg-info i fields ...)
  (struct-copy zcpkg-info i
               fields ...))

(module+ test
  (provide dummy-zcpkg-info)
  (require rackunit)

  (define dummy-zcpkg-info
    (zcpkg-info "acme"
                "anvil"
                "heavy"
                0
                '("certified")
                "setup.rkt"
                '("gravity" "dumb-coyote")
                #f
                #f))

  (test-equal? "Create unabbreviated paths"
               (zcpkg-info->relative-path dummy-zcpkg-info)
               (build-path "acme" "anvil" "heavy" "0"))

  (test-equal? "Create unabbreviated paths by default"
               (zcpkg-info->relative-path dummy-zcpkg-info)
               (zcpkg-info->relative-path dummy-zcpkg-info #:abbrev 0))


  (test-equal? "Create slightly abbreviated paths"
               (zcpkg-info->relative-path dummy-zcpkg-info #:abbrev 1)
               (build-path "acme" "anvil" "heavy"))

  (test-equal? "Create moderately abbreviated paths"
               (zcpkg-info->relative-path dummy-zcpkg-info #:abbrev 2)
               (build-path "acme" "anvil"))

  (test-equal? "Create heavily abbreviated paths"
               (zcpkg-info->relative-path dummy-zcpkg-info #:abbrev 3)
               (build-path "anvil"))

  (test-equal? "Prevent over-abbreviation"
               (zcpkg-info->relative-path dummy-zcpkg-info #:abbrev 10000)
               (zcpkg-info->relative-path dummy-zcpkg-info #:abbrev 3))

  (test-equal? "Cap negative abbreviation"
               (zcpkg-info->relative-path dummy-zcpkg-info #:abbrev -12)
               (zcpkg-info->relative-path dummy-zcpkg-info #:abbrev 0))

  (test-case "zcpkg-info I/O"
    (define-values (i o) (make-pipe))
    (write-zcpkg-info dummy-zcpkg-info o)
    (flush-output o)
    (close-output-port o)
    (check-equal? (read-zcpkg-info i) dummy-zcpkg-info)))
