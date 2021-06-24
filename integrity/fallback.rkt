#lang racket/base

; Define fallback CHFs
;
; ("+" = pro, "-" = con)
;
; + Always available
; + Always compatible
; - Slower
; - All known implementations produce collisions
; - Unvetted by InfoSec experts
;
; Use cases:
;  * Follow the documentation on an unsupported architecture.
;  * Increase availability when operations are not security-critical.

(require racket/contract
         file/sha1
         "chf.rkt")

(provide
 (contract-out
  [fallback-chf-available?
   predicate/c]
  [fallback-make-digest
   (-> input-port? symbol? bytes?)]))

(define (fallback-chf-available? s)
  (and (symbol? s)
       (fallback-find-implementation s)
       #t))

(define (fallback-find-implementation s)
  (case s
    [(sha1) sha1-bytes]
    [else #f]))

(define (fallback-make-digest in algorithm)
  (define md (fallback-find-implementation algorithm))
  (if md
      (md in)
      (raise ($chf-unavailable algorithm))))

(module+ test
  (require rackunit
           "../codec.rkt")

  (define data #"the rain in spain falls mainly on the plain\n")
  (define test-digests
    '((sha1 . "89mymPTvJIenhzZpwZ7uDKUANd8=")))

  (define (reencode x)
    (coerce-string
     (encode 'base64
             (fallback-make-digest
              (open-input-bytes data)
              x))))

  (for ([pair (in-list test-digests)])
    (check-equal? (reencode (car pair))
                  (cdr pair))))
