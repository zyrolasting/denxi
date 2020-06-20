#lang racket/base

(provide generate-account!
         read-account
         (struct-out account))

(require racket/random
         "db/db.rkt")

(struct account (id))

; Use absolute value of signed variant to ensure we don't exceed SQLite's INTEGER maximum.
(define (make-account-id)
  (abs (integer-bytes->integer (crypto-random-bytes 8) #t)))

(define (generate-account!)
  (define acc (account (make-account-id)))
  (write-account acc)
  acc)

(define (write-account acc)
  (define db (connect-db))
  (db query-exec "insert into accounts values ($1)" (account-id acc)))

(define (read-account id)
  (define db (connect-db))
  (define row (db query-row "select id from accounts where id = $1" id))
  (account id))
