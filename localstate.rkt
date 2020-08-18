#lang racket/base

(require racket/contract)

(define input-path/c
  (and/c (not/c complete-path?)
         path-string?))

(provide (contract-out
          [declare-input
           (-> bytes? input-path/c void?)]))

(require "db.rkt"
         "file.rkt"
         "output.rkt"
         "path.rkt"
         "printer.rkt"
         "workspace.rkt"
         "xiden-messages.rkt")

(define (get-localstate-path)
  (build-workspace-path "var/xiden/db"))

(define (connect)
  (define db-path (get-localstate-path))
  (make-directory* (path-only db-path))
  (sqlite3-connect #:database db-path
                   #:mode 'create
                   #:use-place #f))

(define-syntax-rule (define-state-procedure (sig ...) body ...)
  (define (sig ...) (unless (current-db-connection) (initialize!)) body ...))

(define-state-procedure (declare-input digest path)
  (with-handlers ([exn:fail:sql?
                   (λ (e)
                     (if (eq? (exn:fail:sql-sqlstate e) 'constraint)
                         (raise-user-error 'declare-input
                                           "Cannot redeclare input ~a."
                                           path)
                         (raise e)))])
    (query-exec+ "insert into inputs values (?, ?);"
                 path
                 digest)
    (write-output ($declare-input digest path))))

(define (delete-localstate!)
  (delete-file* (get-localstate-path)))

(define (initialize!)
  (write-output ($verbose ($init-localstate (get-localstate-path))))
  (current-db-connection (connect))
  (for ([create-query (in-list create-queries)])
    (query-exec+ create-query)))


(define create-input-table #<<EOS
CREATE TABLE IF NOT EXISTS inputs (
       path TEXT NOT NULL PRIMARY KEY,
       digest BLOB NOT NULL
);
EOS
)


(define create-derivation-table-query #<<EOS
CREATE TABLE IF NOT EXISTS derivations (
       id INTEGER NOT NULL PRIMARY KEY,
       name TEXT,
       digest BLOB,
       errors TEXT
);
EOS
)


(define create-queries
  (list create-input-table
        create-derivation-table-query))
