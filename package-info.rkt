#lang racket/base

; Define a package definition format.

(provide (struct-out package-info)
         make-package-info
         read-package-info-from-directory
         read-package-info
         package-info->hash)

(require "config.rkt"
         "contract.rkt"
         "output.rkt"
         "racket-version.rkt"
         "string.rkt"
         "url.rkt"
         "workspace.rkt"
         "xiden-messages.rkt")


(struct package-info
  (provider-name      ; The name of the package provider
   package-name       ; The name of the package
   edition-name       ; The name of a design used in the package
   revision-number    ; The number of a design's implementation.
   revision-names     ; Aliases for the revision-number.
   inputs             ; A list of input-info
   outputs            ; A list of output-info
   racket-versions    ; A list of supported Racket version ranges
   tags               ; A list of strings for discovery purposes
   description        ; A string describing the package
   home-page)         ; A related link to a project's home page
  #:prefab)


(define (make-package-info
         #:provider-name provider-name
         #:package-name package-name
         #:edition-name [edition-name "draft"]
         #:revision-number [revision-number 0]
         #:revision-names [revision-names null]
         #:outputs [outputs null]
         #:inputs [inputs null]
         #:racket-versions [racket-versions null]
         #:tags [tags null]
         #:description [description #f]
         #:home-page [home-page #f])
  (package-info
   provider-name
   package-name
   edition-name
   revision-number
   revision-names
   inputs
   outputs
   racket-versions
   tags
   description
   home-page))


(define (read-package-info in)
  (define lookup (load-config in))
  (package-info
   (lookup 'provider name-string?)
   (lookup 'package name-string?)
   (lookup 'edition name-string?)
   (lookup 'revision-number exact-nonnegative-integer?)
   (lookup 'revision-names (listof name-string?) null)
   (lookup 'inputs list? null)
   (lookup 'outputs list? null)
   (lookup 'racket-versions racket-version-ranges/c null)
   (lookup 'tags (listof string?) null)
   (lookup 'description (or/c #f string?) #f)
   (lookup 'home-page (or/c #f url-string?) #f)))


(define (package-info->hash pkginfo)
  (hash 'provider (package-info-provider-name pkginfo)
        'package  (package-info-package-name pkginfo)
        'revision-names (package-info-revision-names pkginfo)
        'revision-number (package-info-revision-number pkginfo)
        'inputs (package-info-inputs pkginfo)
        'outputs (package-info-outputs pkginfo)
        'racket-versions (package-info-racket-versions pkginfo)
        'tags (package-info-tags pkginfo)
        'description (package-info-description pkginfo)
        'home-page (package-info-home-page pkginfo)))


(define (read-package-info-from-directory dir)
   (with-handlers ([exn:fail:filesystem?
                    (λ (e) (output-return #f ($package-directory-has-unreadable-info dir)))])
     (output-unit (read-package-info (build-path dir CONVENTIONAL_PACKAGE_INFO_FILE_NAME)))))


(define-syntax-rule (copy-package-info i fields ...)
  (struct-copy package-info i
               fields ...))


(module+ test
  (provide dummy-package-info)
  (require rackunit)

  (define dummy-package-info
    (make-package-info
     #:provider-name "acme"
     #:package-name "anvil"
     #:edition-name "heavy"
     #:revision-names '("certified")
     #:outputs '("setup.rkt")
     #:inputs '("gravity" "dumb-coyote"))))
