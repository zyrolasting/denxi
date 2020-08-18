#lang racket/base

(provide (all-from-out db)
         query-row+
         query-rows+
         query-maybe-row+
         query-value+
         query-maybe-value+
         query-list+
         query-exec+
         in-query+
         start-transaction!
         end-transaction!
         rollback-transaction!
         current-db-connection)

(require db)

(define current-db-connection (make-parameter #f))

(define (start-transaction!)
  (with-handlers
    ([exn:fail:sql?
      (λ (e)
        (define error-info (exn:fail:sql-info e))
        (if (regexp-match? #rx"transaction within a transaction"
                           (cdr (assoc 'message error-info)))
            'transaction-already-running
            (raise e)))])
    (query-exec+ "begin exclusive transaction;")))

(define (rollback-transaction!)
  (with-handlers ([exn:fail:sql?
                   (λ (e)
                     ; A useless rollback should not interrupt the user,
                     ; but should come to a programmer's attention.
                     (if (regexp-match? #rx"no transaction is active" (exn-message e))
                         'needless
                         (raise e)))])
    (query-exec+ "rollback transaction;")))

(define (end-transaction!)
  (query-exec+ "commit transaction;"))

(define (with-connection f)
  (make-keyword-procedure
   (λ (k a . formals)
     (keyword-apply f k a (current-db-connection)
                    formals))))

(define query-exec+        (with-connection query-exec))
(define query-rows+        (with-connection query-rows))
(define query-list+        (with-connection query-list))
(define query-row+         (with-connection query-row))
(define query-maybe-row+   (with-connection query-maybe-row))
(define query-value+       (with-connection query-value))
(define query-maybe-value+ (with-connection query-maybe-value))
(define in-query+          (with-connection in-query))
