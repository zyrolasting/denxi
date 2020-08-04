#lang racket/base

; Define and generate information about an edition, namely the next
; available revision number and all reserved revision names.  This
; determines if a submitted package definition is acceptable.

(provide (all-defined-out))

(require "config.rkt"
         "file.rkt"
         "zcpkg-info.rkt")

(struct edition-info (next-revision index) #:prefab)

(define (available-revision-number? ed n)
  (eq? n (edition-info-next-revision ed)))

(define (available-revision-name? ed name)
  (not (hash-has-key? (edition-info-index ed) name)))

(define (read-edition-info in)
  (define lookup (load-config in))
  (edition-info (lookup 'next-revision exact-nonnegative-integer? 0)
                (lookup 'index hash? (hash))))

(define (edition-info->hash info)
  (hash 'next-revision (edition-info-next-revision info)
        'index (edition-info-index info)))

(define (write-edition-info info o)
  (save-config!
   (make-config-closure
    (edition-info->hash info)
    '(next-revision index))
   o))

(define (generate-edition-info infos)
  (call-with-values
   (λ ()
     (for/fold ([next-revision 0]
                [wip (hash)])
               ([info infos])
       (values (add1 next-revision)
               (apply hash-set* wip
                      (foldl (λ (name args)
                               (cons name (cons (zcpkg-info-revision-number info) args)))
                             null
                             (zcpkg-info-revision-names info))))))
   edition-info))

(module+ test
  (require rackunit)

  (define dummy-edition-info (edition-info 7 (hash "a" 0)))

  (test-true "Accept only the next available revision number"
             (and (available-revision-number? dummy-edition-info 7)
                  (not (available-revision-number? dummy-edition-info 0))
                  (not (available-revision-number? dummy-edition-info -1))
                  (not (available-revision-number? dummy-edition-info 6))
                  (not (available-revision-number? dummy-edition-info 8))))

  (test-true "Accept only unused revision names"
             (and (available-revision-name? dummy-edition-info "b")
                  (not (available-revision-name? dummy-edition-info "a"))))

  (test-equal? "Generate edition info from package definitions"
               (generate-edition-info
                (in-list
                 (list
                  (make-zcpkg-info #:provider-name "a"
                                   #:package-name "b"
                                   #:revision-number 0
                                   #:revision-names '("x" "y"))
                  (make-zcpkg-info #:provider-name "a"
                                   #:package-name "b"
                                   #:revision-number 1
                                   #:revision-names '("q" "r")))))
               (edition-info 2 (hash "x" 0 "y" 0 "q" 1 "r" 1)))

  (test-case "edition-info I/O"
    (define-values (i o) (make-pipe))
    (write-edition-info dummy-edition-info o)
    (flush-output o)
    (close-output-port o)
    (check-equal? (read-edition-info i) dummy-edition-info)))
