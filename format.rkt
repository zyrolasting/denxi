#lang racket/base

; Extend racket/format

(require racket/contract
         racket/format
         racket/pretty
         racket/string
         racket/struct
         "stream.rkt")

(provide (all-from-out racket/format)
         (contract-out
          [format-value
           (-> any/c string?)]
          [indent-lines
           (-> (listof string?) (listof string?))]
          [join-lines
           (->* ((listof string?)) (#:trailing? any/c #:suffix (or/c char? string? #f)) string?)]))


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


(module+ test
  (require racket/format
           rackunit)

  (check-equal? (indent-lines '("a" "b")) '("  a" "  b"))
  (check-equal? (join-lines #:suffix "\n" '("a" "b" "c")) "a\nb\nc"))
