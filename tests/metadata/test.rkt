#lang racket/base

(require racket/random
         racket/function
         racket/format
         racket/runtime-path
         racket/generator
         racket/string
         rackunit
         net/head
         zcpkg/zcpkg-info
         (for-syntax racket/base
                     racket/match
                     racket/string
                     racket/struct-info))



; Test comes with #lang info files.
(define-runtime-path public.rkt "public.rkt")
(define-runtime-path blank "blank.rkt")

; Expected structure data.
(define expected-zcpkg-info-fields
  '("janedoe"
    "my-project"
    "draft"
    12
    ((0 "initial"))
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


(module+ test
  (define blank-instance (apply zcpkg-info (build-list (procedure-arity zcpkg-info) (const #f))))
  (define expected-instance (apply zcpkg-info expected-zcpkg-info-fields))

  (test-equal? "A blank info file makes a blank instance"
               (read-zcpkg-info blank)
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
