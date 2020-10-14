#lang racket/base

(require racket/format
         racket/match
         racket/pretty
         racket/string
         "contract.rkt"
         "message.rkt")

(define message-formatter/c (-> $message? string?))

(provide (all-from-out "contract.rkt" ; For define+provide expansion
                       racket/format)
         define-message-formatter
         define+provide-message-formatter
         message-formatter
         (contract-out
          [format-symbol-for-display
           (-> symbol? string?)]
          [indent-lines
           (-> (listof string?) (listof string?))]
          [join-lines
           (-> (listof string?) string?)]
          [join-lines*
           (->* () #:rest (listof string?) string?)]
          [message-formatter/c
           contract?]
          [current-message-formatter
           (parameter/c message-formatter/c)]
          [format-message
           (-> $message? string?)]
          [combine-message-formatters
           (->* () #:rest (listof message-formatter/c) message-formatter/c)]))



;-----------------------------------------------------------------------------
; Conventional formatting procedures

(define (format-symbol-for-display i)
  (format "`~a`" i))

(define (indent-lines lines)
  (map (λ (s) (~a "  " s)) lines))

(define (join-lines lines)
  (string-join lines "\n"))

(define (join-lines* . lines)
  (join-lines lines))


;-----------------------------------------------------------------------------
; $message formatters

(define (combine-message-formatters . formatters)
  (λ (m) (format-message-aux m formatters)))

(define (format-message-aux m formatters)
  (if (null? formatters)
      (error 'format-message "Unknown message type: ~s" m)
      (with-handlers ([exn:misc:match? (λ (e) (format-message-aux m (cdr formatters)))])
        ((car formatters) m))))

(define-syntax-rule (message-formatter patts ...)
  (λ (m) (match m patts ...)))

(define-syntax-rule (define-message-formatter id patts ...)
  (define id (message-formatter patts ...)))

(define-syntax-rule (define+provide-message-formatter id patts ...)
  (begin (provide (contract-out [id message-formatter/c]))
         (define-message-formatter id patts ...)))

(define+provide-message-formatter default-message-formatter
  [($show-string v) v]
  [($show-datum v) (pretty-format #:mode 'write v)]
  [v (~s v)])

(define current-message-formatter
  (make-parameter default-message-formatter))

(define (format-message m)
  ((current-message-formatter) m))

(module+ test
  (require racket/format
           rackunit
           "setting.rkt")

  (check-equal? (format-symbol-for-display 'foo) "`foo`")
  (check-equal? (indent-lines '("a" "b")) '("  a" "  b"))
  (check-equal? (join-lines '("a" "b" "c")) "a\nb\nc")
  (check-equal? (join-lines* "a" "b" "c") "a\nb\nc")

  (define dummy ($show-string "Testing: Blah"))

  (test-case "Use fallback strings for message formatting"
    (check-equal? (format-message dummy)
                  ($show-string-message dummy))
    (check-equal? (format-message ($show-datum '(1 (2) 3)))
                  (pretty-format #:mode 'write '(1 (2) 3)))
    (check-equal? (format-message '(1 (2) 3))
                  (~s '(1 (2) 3))))

  (test-case "Compose message formatters"
    (define-message $other (message))
    (define-message-formatter upper [($show-string v) (string-upcase v)])
    (define-message-formatter writer [($show-datum v) (format-message ($show-string (~s v)))])
    (parameterize ([current-message-formatter (combine-message-formatters upper writer)])
      (check-equal? (format-message ($show-string "foo")) "FOO")
      (check-equal? (format-message ($show-datum  "bar")) "\"BAR\"")
      (check-exn
       exn:fail?
       (λ () (format-message ($other "whatever")))))))
