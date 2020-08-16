#lang racket/base

(require racket/function
         db
         "file.rkt"
         "format.rkt"
         "provider-info.rkt"
         "workspace.rkt"
         "zcpkg-info.rkt")

#|

CRUD provider
CRU  package

|#


(define current-db-connection (make-parameter #f))

(define (connect)
  (sqlite3-connect #:database (build-workspace-path "zcpkg.db")
                   #:mode 'create
                   #:use-place #f))

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

(define (start-zcpkg-database!)
  (current-db-connection (connect))
  (query-exec+ create-providers-stmt)
  (query-exec+ create-sessions-stmt)
  (query-exec+ create-packages-stmt)
  (query-exec+ create-revision-names-stmt))

(define (with-connection f)
  (λ args (apply f (current-db-connection) args)))

(define query-exec+ (with-connection query-exec))
(define query-rows+ (with-connection query-rows))
(define query-list+ (with-connection query-list))
(define query-row+ (with-connection query-row))
(define query-maybe-row+ (with-connection query-maybe-row))
(define query-value+ (with-connection query-value))
(define query-maybe-value+ (with-connection query-maybe-value))


(define (in-packages)
  (sequence-map values
                (in-query (current-db-connection)
                          "select * from packages;"
                          #:fetch 100)))


(define (add-revision-names package-id info)
  (for ([name (in-list (zcpkg-info-revision-names info))])
    (query-exec+ "INSERT INTO revision_names VALUES (?, ?)"
                 package-id
                 name)))


(define (insert-provider info)
  (query-exec+ insert-provider-stmt
               (provider-info-name info)
               (provider-info-email info)
               (provider-info-public-key info)))


(define (insert-package provider-id info)
  (query-exec+ insert-package-stmt
               provider-id
               (zcpkg-info-package-name info)
               (zcpkg-info-edition-name info)
               (zcpkg-info-revision-number info)
               (~a (bytes->string/utf-8 (zcpkg-info-integrity info)) ".tgz")
               (or (zcpkg-info-signature info) sql-null)))


(define create-providers-stmt #<<EOS
CREATE TABLE IF NOT EXISTS providers (
       id INTEGER PRIMARY KEY NOT NULL,
       name TEXT,
       email TEXT,
       pubkey BLOB
);
EOS
)


(define insert-provider-stmt #<<EOS
INSERT INTO providers VALUES (NULL, ?, ?, ?);
EOS
)


(define create-sessions-stmt #<<EOS
CREATE TABLE IF NOT EXISTS sessions (
       id INTEGER PRIMARY KEY NOT NULL,
       provider_id INTEGER NOT NULL,
       FOREIGN KEY(provider_id) REFERENCES providers(id)
);
EOS
)


(define create-packages-stmt #<<EOS
CREATE TABLE IF NOT EXISTS packages (
       id INTEGER PRIMARY KEY NOT NULL,
       provider_id INTEGER,
       name TEXT,
       edition TEXT,
       revision_number INTEGER,
       artifact_name TEXT,
       signature BLOB,
       FOREIGN KEY(provider_id) REFERENCES providers(id)
);
EOS
)


(define insert-package-stmt #<<EOS
INSERT INTO packages VALUES (NULL, ?, ?, ?, ?, ?);
EOS
)


(define create-revision-names-stmt #<<EOS
CREATE TABLE IF NOT EXISTS revision_names (
       package_id INTEGER NOT NULL,
       name TEXT,
       FOREIGN KEY(package_id) REFERENCES packages(id)
);
EOS
)
