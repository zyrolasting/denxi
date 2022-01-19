#lang racket/base

; Extend racket/format

(require racket/contract
         racket/format
         racket/match
         racket/pretty
         racket/string
         racket/struct
         "stream.rkt"
         "message.rkt")

(define message-formatter/c (-> $message? string?))

(provide (all-from-out racket/contract ; For define+provide expansion
                       racket/format)
         define-message-formatter
         define+provide-message-formatter
         message-formatter
         (contract-out
          [format-value
           (-> any/c string?)]
          [indent-lines
           (-> (listof string?) (listof string?))]
          [join-lines
           (->* ((listof string?)) (#:trailing? any/c #:suffix (or/c char? string? #f)) string?)]
          [join-lines*
           (->* () (#:trailing? any/c #:suffix (or/c char? string? #f)) #:rest (listof string?) string?)]
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

(define system-eol
  (if (equal? (system-type 'os) 'windows)
      "\r\n"
      "\n"))

(define (indent-lines lines)
  (map (λ (s) (~a "  " s)) lines))

(define (join-lines #:suffix [preferred-suffix #f] #:trailing? [trailing? #f] lines)
  (define suffix (select-eol preferred-suffix))
  (define joined (string-join lines suffix))
  (if trailing?
      (string-append joined suffix)
      joined))

(define (join-lines* #:suffix [preferred-suffix #f] #:trailing? [trailing? #f] . lines)
  (join-lines #:suffix preferred-suffix #:trailing? trailing? lines))


(define (select-eol preferred-suffix)
  (cond [(not preferred-suffix) system-eol]
        [(char? preferred-suffix) (string preferred-suffix)]
        [(string? preferred-suffix) preferred-suffix]))


(define (format-value v)
  (cond [(string? v) v] ; terminal case
        [(symbol? v)
         (~v v)]
        [(stream? v)
         (format-stream v null)]
        [(srcloc? v)
         (srcloc->string v)]
        [(number? v) ; must come before sequence?
         (~s v)]
        [(sequence? v)
         (format-value (sequence->stream v))]
        [(pair? v)
         (format "(~a . ~a)"
                 (format-value (car v))
                 (format-value (cdr v)))]
        [(continuation-mark-set? v)
         (format-value (continuation-mark-set->context v))]
        [(struct? v)
         (format-value (in-vector (struct->vector v)))]
        [else
         (let ([as-string (~s v)])
           (if (string-prefix? as-string "#<")
               (~s as-string)
               as-string))]))


(define (format-stream s [output null])
  (if (stream-empty? s)
      (format-string-list "(" ")" output)
      (format-stream (stream-rest s)
                     (cons (format-values (λ () (stream-first s)))
                           output))))

(define (format-string-list open-bracket close-bracket elements)
  (define element-count
    (length elements))
    
  (define spaced
    (for/fold ([spaced* null] #:result (reverse spaced*))
              ([(element i) (in-indexed (in-list elements))])
      (if (= i (sub1 element-count))
          (cons element spaced*)
          (cons " " (cons element spaced*)))))
               
  (define with-closed
    (cons close-bracket spaced))

  (define with-both
    (cons open-bracket (reverse with-closed)))

  (apply string-append with-both))


(define (format-values make-values)
  (call-with-values make-values
                    (λ vs (format-string-list "[" "]"
                                              (map format-value vs)))))


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
  [v (format-value v)])

(define current-message-formatter
  (make-parameter default-message-formatter))

(define (format-message m)
  ((current-message-formatter) m))

(module+ test
  (require racket/format
           rackunit
           "setting.rkt")

  (check-equal? (indent-lines '("a" "b")) '("  a" "  b"))
  (check-equal? (join-lines #:suffix "\n" '("a" "b" "c")) "a\nb\nc")
  (check-equal? (join-lines* #:suffix #\| #:trailing? #t "a" "b" "c") "a|b|c|")

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
