#lang racket/base

(provide write-party
         read-account)

(require "db/db.rkt"
         "account.rkt")

(struct party (create-time account-id public-key))

(define (make-table!)
  (define db (connect-db))
  (db query-exec #<<SQL
CREATE TABLE IF NOT EXISTS parties (
id PRIMARY KEY AUTOINCREMENT,
create_time INTEGER,
account_id INTEGER,
public_key STRING,
FOREIGN KEY(account_id) REFERENCES account(id)
)
SQL
))

(define (write-party acc pubkey)
  (define db (connect-db))
  (db query-exec "insert into parties values ($1)"
      (account-id acc)))

(define (read-parties acc)
  (define db (connect-db))
  (db query-rows/apply
      party
      "select id from parties where account_id = $1"
      (account-id acc)))
