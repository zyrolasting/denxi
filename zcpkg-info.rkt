#lang racket/base

; Define metadata about a zero-collection package.

(provide (all-defined-out))

(require racket/file
         version/utils
         (only-in racket/format ~a)
         (only-in racket/list drop-right)
         "contract.rkt"
         "config.rkt"
         "string.rkt"
         "url.rkt"
         "workspace.rkt"
         "zcpkg-settings.rkt")

(struct zcpkg-info
  (provider-name      ; The name of the package provider
   package-name       ; The name of the package itself
   edition-name       ; The name of a design used in the package
   revision-number    ; The number of a design's implementation.
   revision-names     ; Aliases for the revision-number.
   setup-module       ; A Racket module responsible for userspace changes.
   dependencies       ; A list of dependency queries.
   integrity          ; A digest used to verify package contents
   signature          ; A signature used to authenticate the provider
   racket-versions    ; A list of supported Racket version ranges
   tags               ; A list of strings for discovery purposes
   description        ; A string describing the package
   home-page          ; A related link to a project's home page
   launchers)         ; A list of launcher specifications
  #:prefab)

(define (make-zcpkg-info
         #:provider-name provider-name
         #:package-name package-name
         #:edition-name [edition-name "draft"]
         #:revision-number [revision-number 0]
         #:revision-names [revision-names null]
         #:setup-module [setup-module #f]
         #:dependencies [dependencies null]
         #:integrity [integrity #f]
         #:signature [signature #f]
         #:racket-versions [racket-versions null]
         #:tags [tags null]
         #:description [description #f]
         #:home-page [home-page #f]
         #:launchers [launchers null])
  (zcpkg-info
   provider-name
   package-name
   edition-name
   revision-number
   revision-names
   setup-module
   dependencies
   integrity
   signature
   racket-versions
   tags
   description
   home-page
   launchers))


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
              (lookup 'signature (or/c #f bytes?) #f)
              (lookup 'racket-versions (listof (cons/c valid-version? valid-version?)) null)
              (lookup 'tags (listof string?) null)
              (lookup 'description (or/c #f string?) #f)
              (lookup 'home-page (or/c #f url-string?) #f)
              (lookup 'launchers (listof hash?) null)))

(define (add-launcher-spec-defaults spec)
  (hasheq 'args (hash-ref spec 'args null)
          'gracket? (hash-ref spec 'gracket? #f)
          'name (hash-ref spec 'name #f)
          'collects (hash-ref spec 'collects (hasheq))
          'aux-path (hash-ref spec 'aux-path CONVENTIONAL_LAUNCHER_AUX_DIRECTORY_NAME)))

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
        'signature (zcpkg-info-signature info)
        'racket-versions (zcpkg-info-racket-versions info)
        'tags (zcpkg-info-tags info)
        'description (zcpkg-info-description info)
        'home-page (zcpkg-info-home-page info)
        'launchers (zcpkg-info-launchers info)))

(define (write-zcpkg-info-to-directory info dir)
  (make-directory* dir)
  (call-with-output-file #:exists 'truncate/replace
    (build-path dir CONVENTIONAL_PACKAGE_INFO_FILE_NAME)
    (λ (o) (write-zcpkg-info info o))))

(define (write-zcpkg-info info o)
  (save-config!
   (make-config-closure
    (zcpkg-info->hash info)
    '(package
      tags
      description
      home-page
      provider
      edition
      revision-number
      revision-names
      setup-module
      dependencies
      integrity
      signature
      racket-versions))
   o))

(define-syntax-rule (copy-zcpkg-info i fields ...)
  (struct-copy zcpkg-info i
               fields ...))

(module+ test
  (provide dummy-zcpkg-info)
  (require rackunit)

  (define dummy-zcpkg-info
    (make-zcpkg-info
     #:provider-name "acme"
     #:package-name "anvil"
     #:edition-name "heavy"
     #:revision-names '("certified")
     #:setup-module "setup.rkt"
     #:dependencies '("gravity" "dumb-coyote")))

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

  (test-equal? "Add default values to launcher spec"
               (add-launcher-spec-defaults (hash))
               (hasheq 'args null
                       'gracket? #f
                       'name #f
                       'collects (hasheq)
                       'aux-path CONVENTIONAL_LAUNCHER_AUX_DIRECTORY_NAME))

  (let ([spec (hasheq 'args 1 'gracket? 2 'name 3 'collects 4 'aux-path 5)])
    (test-equal? "Override each default value of launcher spec"
                 (add-launcher-spec-defaults spec)
                 spec))

  (test-case "zcpkg-info I/O"
    (define-values (i o) (make-pipe))
    (write-zcpkg-info dummy-zcpkg-info o)
    (flush-output o)
    (close-output-port o)
    (check-equal? (read-zcpkg-info i) dummy-zcpkg-info)))
