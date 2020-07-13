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



(module+ test
  (require racket/file
           racket/format
           racket/function
           net/head
           rackunit
           (for-syntax racket/base
                       racket/match
                       racket/string
                       racket/struct-info))

  (define (display-to-temp-file content)
    (define path (make-temporary-file "~a"))
    (display-to-file #:exists 'truncate/replace content path)
    path)


  (define (get-public.rkt)
    (display-to-temp-file #<<EOF
#lang info

(define edition-name "draft")
(define revision-names '("initial"))
(define installer "installer.rkt")
(define dependencies
  '("zyrolasting/unlike-assets/lib/draft/0"
    "jondoe/cool/doc/draft/marvelous"))

(define package-name "my-project")
(define revision-number 12)
(define provider-name "janedoe")
(define integrity #"12345678")
(define signature #"87654321")
(define upload-timestamp 45758627)

EOF
))

(define (get-blank.rkt)
  (display-to-temp-file "#lang info\n"))

; Expected structure data.
(define expected-zcpkg-info-fields
  '("janedoe"
    "my-project"
    "draft"
    12
    ("initial")
    "installer.rkt"
    ("zyrolasting/unlike-assets/lib/draft/0" "jondoe/cool/doc/draft/marvelous")
    #"12345678"
    #"87654321"
    45758627))

; Generate a string of HTTP headers that act like the equivalent of a
; populated info.rkt file.

(define-syntax (accessor-names stx)
  (match-define (list _ _ _ accessors _ _)
    (extract-struct-info (syntax-local-value #'zcpkg-info)))
  (define identifier->string (compose symbol->string syntax->datum))
  (datum->syntax stx `(list . ,(map identifier->string (reverse accessors)))))

(define headers
  (foldl
   (λ (accessor-name value wip)
     (insert-field (~a accessor-name)
                   (~s value)
                   wip))
   empty-header
   accessor-names
   expected-zcpkg-info-fields))

  (define blank-instance (apply zcpkg-info (build-list (procedure-arity zcpkg-info) (const #f))))
  (define expected-instance (apply zcpkg-info expected-zcpkg-info-fields))
  (define public.rkt (get-public.rkt))
  (define blank.rkt (get-blank.rkt))
  (dynamic-wind void
                (λ ()
                  (test-equal? "A blank info file makes a blank instance"
                               (read-zcpkg-info blank.rkt)
                               blank-instance)

                  (test-equal? "A blank input port makes a blank instance"
                               (read-zcpkg-info (open-input-string ""))
                               blank-instance)

                  (test-equal? "Read HTTP headers"
                               (read-zcpkg-info (open-input-string headers))
                               expected-instance)

                  (test-equal? "Read #lang info file"
                               (read-zcpkg-info public.rkt)
                               expected-instance))
                (λ ()
                  (delete-file public.rkt)
                  (delete-file blank.rkt))))
