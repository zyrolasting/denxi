#lang racket/base

; Define a small text protocol for exchanging Racket hashes over ports.

(require racket/contract
         racket/fasl
         racket/generator
         racket/hash
         racket/match
         racket/pretty
         racket/sequence
         racket/serialize)

(define output-format?
  (or/c 'datum 'fasl 'report))

(provide
 <<
 (contract-out
  [write-output (->* (hash?) (#:format output-format? output-port?) void?)]
  [read-output (-> any/c hash?)]))

(define current-output-format (make-parameter 'datum))

(define (write-output v #:format [fmt (current-output-format)] [out (current-output-port)])
  (when ((current-hash-predicate) v)
    (parameterize ([current-output-port out])
      (printf "~s " fmt)
      ((case fmt
         [(fasl) write-fasl]
         [(datum) write-datum]
         [(report) write-report])
       v)
      (flush-output))))


(define (read-output in)
  (define v (read in))
  (if (eof-object? v) v
      (case v
        [(datum) (read in)]
        [(fasl) (read-fasl in)]
        [else (error 'read-output
                     "Unsupported value in port: ~a"
                     v)])))


(define (in-output [in (current-input-port)])
  (in-port read-output in))

(define <<
  (make-keyword-procedure
   (λ (kws kwas fmt-message . args)
     (write-output
      (hash-set (make-immutable-hash
                 (map (λ (kw a) (cons (string->symbol (keyword->string kw)) a))
                      kws
                      kwas))
                'message
                (apply format fmt-message args))))))


(define (rewrite-output [in (current-input-port)])
  (sequence-for-each write-output (in-output in)))

(define (write-fasl v)
  (fasl->s-exp (serialize v) (current-output-port)))

(define (read-fasl in)
  (deserialize (fasl->s-exp in)))

(define (write-datum v)
  (pretty-write #:newline? #t v))

(define (write-report v)
  (define message (hash-ref v 'message "not set"))
  (define no-msg (hash-remove v 'message))

  (define (write-entry k v)
    (printf "~a := ~s~n" k v))

  (write-entry 'message message)
  (for ([(k v) (in-hash no-msg)])
    (write-entry k v))
  (display (newline)))

(define (read-report in [h (hash)])
  (define next (read-line in))
  (if (equal? next "")
      h
      (read-report in
                   (hash-set h
                             (read in)
                             (begin (read in) ; discard :=
                                    (read in))))))


(define pretty-output?
  (make-parameter #f (λ (v) (and v #t))))

(define current-hash-predicate
  (make-parameter (λ _ #t)))

(module+ test
  (require rackunit)

  (define-values (i o) (make-pipe))

  (define h (hash 'message "yay"))
  (write-output h o)
  (test-equal? "Can reproduce"
               (read-output i)
               h))



(module+ main
  (require racket/cmdline)
  (command-line
   #:once-any
   [("--fasl")
    "Use fast-load serialization for printed data"
    (current-output-format 'fasl)]

   [("--write")
    "Use write to print data"
    (current-output-format 'datum)]

   [("--report")
    "Use write-report to print data"
    (current-output-format 'report)]

   #:args ()
   (rewrite-output)))
