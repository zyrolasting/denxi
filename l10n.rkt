#lang racket/base

(provide get-message-formatter)

(require racket/runtime-path
         "printer.rkt")

(define-runtime-path here "l10n")

(define (get-message-formatter)
  (with-handlers ([exn:fail:filesystem? americentric-fallback])
    (combine-message-formatters
     (dynamic-require (get-module-path (system-language+country))
                      'format-message
                      americentric-fallback)
     default-message-formatter)))

(define (get-module-path locale)
  (path-replace-extension
   (build-path here
               (string-downcase
                (regexp-replace
                 #rx"_"
                 (regexp-replace #px"\\..+" locale "")
                 "-")))
   #".rkt"))

(define (americentric-fallback . _)
  (dynamic-require (get-module-path "en-us")
                   'format-message))

(module+ test
  (require rackunit)

  (test-case "Can get formatter on current system"
    (define formatter (get-message-formatter))
    (check-pred procedure? formatter)
    (check-eq? (procedure-arity formatter) 1)))
