#lang racket/base

(require racket/random
         racket/runtime-path
         rackunit
         zcpkg/zcpkg-info)

(define-runtime-path meta "public.rkt")
(define-runtime-path blank "blank.rkt")

(module+ test
  (test-case "Can read #lang info into structs"
    (check-equal?
     (read-zcpkg-info/public meta)
     (zcpkg-info/public
      "lib"
      "draft"
      "initial"
      "installer.rkt"
      '(zyrolasting/unlike-assets/lib/draft/0 jondoe/cool/doc/draft/marvelous)
      "my-project"
      12
      "janedoe"
      #"12345678"
      #"87654321"
      45758627))
    (check-equal?
     (read-zcpkg-info meta)
     (zcpkg-info
      "lib"
      "draft"
      "initial"
      "installer.rkt"
      '(zyrolasting/unlike-assets/lib/draft/0 jondoe/cool/doc/draft/marvelous))))
  (test-equal? "Blanks are #f"
    (read-zcpkg-info/public blank)
    (zcpkg-info/public #f #f #f #f #f #f #f #f #f #f #f)))
