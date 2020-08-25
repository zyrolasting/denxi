#lang racket/base

; Define a package definition format.

(require "contract.rkt")

(provide (struct-out package-info)
         (struct-out output-info)
         (struct-out input-info)
         make-package-info
         read-package-info
         make-package-name
         package-info->hash
         (contract-out
          [well-formed-output-info/c
           flat-contract?]
          [well-formed-input-info/c
           flat-contract?]))


(require racket/format
         racket/sequence
         "config.rkt"
         "encode.rkt"
         "output.rkt"
         "integrity.rkt"
         "racket-version.rkt"
         "signature.rkt"
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

(struct output-info
  (name                 ; The name of the link used to reference the output of `builder-name`
   builder-name         ; Matches the `input-info-name` of an input used as a Racket build module
   builder-expressions) ; A list of expressions to eval in a sandbox
  #:prefab)

(struct input-info
  (name       ; The name of the link used to reference input bytes
   sources    ; Where to look to get bytes
   integrity  ; Integrity information: Did I get the right bytes?
   signature) ; Signature for authentication: Did the bytes come from someone I trust?
  #:prefab)


(define well-formed-input-info/c
  (struct/c input-info
            non-empty-string?
            (non-empty-listof any/c)
            (or/c #f well-formed-integrity-info/c)
            (or/c #f well-formed-signature-info/c)))


(define well-formed-output-info/c
  (struct/c output-info
            non-empty-string?
            non-empty-string?
            list?))


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


(define (merge-package-info a b)
  (struct-copy package-info b
               [inputs
                (merge-package-lists
                 #:get-name input-info-name
                 (package-info-inputs a)
                 (package-info-inputs b))]
               [outputs
                (merge-package-lists
                 #:get-name output-info-name
                 (package-info-outputs a)
                 (package-info-outputs b))]))


(define (merge-package-lists #:get-name get-name list-a list-b)
  (hash-values (for/fold ([seen (hash)])
                         ([v (sequence-append (in-list list-a) (in-list list-b))])
                 (hash-set seen (get-name v) v))))


(define (read-package-info in)
  (define lookup (load-config in))
  (package-info
   (lookup 'provider name-string?)
   (lookup 'package name-string?)
   (lookup 'edition name-string?)
   (lookup 'revision-number exact-nonnegative-integer?)
   (lookup 'revision-names (listof name-string?) null)
   (lookup 'inputs (listof well-formed-input-info/c) null)
   (lookup 'outputs (listof well-formed-output-info/c) null)
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


(define-syntax-rule (copy-package-info i fields ...)
  (struct-copy package-info i
               fields ...))


(define (make-package-name pkginfo)
  (format "~a-~a"
          (encoded-file-name
           (make-digest (open-input-string (~s pkginfo)) 'sha384))
          (package-info-package-name pkginfo)))


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
