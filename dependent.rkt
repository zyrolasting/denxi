#lang racket/base

(provide (struct-out trace)
         monodependent
         polydependent)

(require racket/contract
         racket/match
         racket/set)

(struct trace
  (location breadcrumbs visited))


(define (polydependent namespace name continue break)
  (define t (trace-capture namespace name))
  (match-define (trace location breadcrumbs visited) t)
  (if (set-member? visited location)
      (break t)
      (let ([t* (trace-extend t (cons namespace name))])
        (with-continuation-mark namespace t*
          (continue t*)))))


(define (monodependent name lookup break [output null])
  (polydependent #f
                 name
                 (λ (trace)
                   (for/fold ([output* output] #:result (cons name output*))
                             ([dep (in-list (lookup name))])
                     (monodependent dep lookup break output*)))
                 break))


(define (trace-capture namespace name)
  (continuation-mark-set-first (current-continuation-marks)
                               namespace
                               (trace name null (set))))


(define (trace-extend t location*)
  (trace location*
         (cons location* (trace-breadcrumbs t))
         (set-add (trace-visited t) location*)))


(module+ test
  (require "test.rkt")
  (test cycle-detection
        (define namespace (gensym))
        (define name (gensym))
        (define (break . _) 'cycle)
        (define (visit . _) (polydependent namespace name visit break))
        (compare eq?
                 'cycle
                 (polydependent namespace
                                name
                                visit
                                break))))
