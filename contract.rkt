#lang racket/base

; Extend racket/contract

(require racket/contract)

(provide (all-from-out racket/contract)
         (contract-out
          [rewrite-contract-error-message
           (-> exn:fail:contract? symbol? exn:fail:contract?)]))

(define (rewrite-contract-error-message e id)
  (struct-copy exn:fail:contract e
               [message #:parent exn
                        (regexp-replace #rx"^[^\n]+"
                                        (exn-message e)
                                        (format "Invalid value for ~a" id))]))
