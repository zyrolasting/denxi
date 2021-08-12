#lang racket/base

; Loads human-readable strings dynamically

(provide get-message-formatter
         get-localized-string
         run+print-subprogram)

(require racket/list
         racket/runtime-path
         racket/sequence
         "codec.rkt"
         "format.rkt"
         "subprogram.rkt"
         "printer.rkt")

(define-runtime-path here "l10n")

(define (run+print-subprogram subprogram-inst)
  (define-values (result messages) (run-subprogram subprogram-inst))
  (write-message-log messages (get-message-formatter))
  result)

(define (dynamic-require/localized key)
  (let ([on-failure (americentric-fallback key)])
    (with-handlers ([exn:fail:filesystem? on-failure])
      (dynamic-require (get-module-path (system-language+country))
                       key
                       on-failure))))

(define (get-message-formatter)
  (define f
    (combine-message-formatters (dynamic-require/localized 'format-message/locale)
                                default-message-formatter))
  (λ (m)
    (parameterize ([current-message-formatter f])
      (f m))))

(define (get-localized-string-lookup)
  (dynamic-require/localized 'get-string))

(define (get-localized-string sym)
  ((get-localized-string-lookup) sym))

(define (get-module-path locale)
  (path-replace-extension
   (build-path here
               (string-downcase
                (coerce-string
                 (regexp-replace
                  #rx"_"
                  (regexp-replace #px"\\..+" locale "")
                  "-"))))
   #".rkt"))

(define (americentric-fallback sym)
  (λ _ (dynamic-require (get-module-path "en-us") sym)))

(module+ test
  (require rackunit
           "message.rkt")

  (test-not-exn "Americentric fallback is always available"
                (λ ()
                  (check-pred procedure? (americentric-fallback 'format-message/locale))
                  (check-pred procedure? (americentric-fallback 'get-string))))

  (test-case "Can get formatter on current system"
    (define formatter (get-message-formatter))
    (check-pred procedure? formatter)
    (check-eq? (procedure-arity formatter) 1)
    (check-equal? (formatter ($show-string "a")) "a")))
