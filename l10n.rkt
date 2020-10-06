#lang racket/base

(provide get-message-formatter
         run+print-log)

(require racket/list
         racket/runtime-path
         racket/sequence
         "message.rkt"
         "printer.rkt")

(define-runtime-path here "l10n")

(define (run+print-log logged-inst)
  (define-values (result messages) (run-log logged-inst))
  (define format-message (get-message-formatter))
  (sequence-for-each
   (λ (m) (write-message m format-message))
   (in-list (reverse (flatten messages))))
  result)

(define (dynamic-require/localized key)
  (let ([on-failure (americentric-fallback key)])
    (with-handlers ([exn:fail:filesystem? on-failure])
      (dynamic-require (get-module-path (system-language+country))
                       key
                       on-failure))))

(define (get-message-formatter)
  (combine-message-formatters (dynamic-require/localized 'format-message)
                              default-message-formatter))

(define (get-string-lookup key)
  (dynamic-require/localized 'get-string))

(define (get-module-path locale)
  (path-replace-extension
   (build-path here
               (string-downcase
                (regexp-replace
                 #rx"_"
                 (regexp-replace #px"\\..+" locale "")
                 "-")))
   #".rkt"))

(define (americentric-fallback sym)
  (λ _ (dynamic-require (get-module-path "en-us") sym)))

(module+ test
  (require rackunit)

  (test-case "Can get formatter on current system"
    (define formatter (get-message-formatter))
    (check-pred procedure? formatter)
    (check-eq? (procedure-arity formatter) 1)))
