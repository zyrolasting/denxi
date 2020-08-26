#lang racket/base

(require racket/contract)

(define input-path/c
  (and/c (not/c complete-path?)
         path-string?))

(provide (contract-out
          [build-object-path
           (-> bytes? complete-path?)]
          [find-latest-package-id
           (-> xiden-query? (or/c #f exact-positive-integer?))]
          [get-derivation-directory
           (-> exact-positive-integer? path-string?)]
          [make-addressable-file
           (-> non-empty-string? input-port? (or/c +inf.0 exact-positive-integer?) complete-path?)]
          [declare-file
           (-> bytes? input-path/c void?)]))

(require "db.rkt"
         "encode.rkt"
         "exn.rkt"
         "file.rkt"
         "format.rkt"
         "integrity.rkt"
         "rc.rkt"
         "output.rkt"
         "package-info.rkt"
         "path.rkt"
         "port.rkt"
         "printer.rkt"
         "query.rkt"
         "string.rkt"
         "workspace.rkt"
         "xiden-messages.rkt")

(define (get-localstate-path)
  (build-workspace-path "var/xiden/db"))

(define (build-object-path digest)
  (build-workspace-path
   "var/xiden/objects"
   (encoded-file-name digest)))

(define (connect)
  (define db-path (get-localstate-path))
  (make-directory* (path-only db-path))
  (sqlite3-connect #:database db-path
                   #:mode 'create
                   #:use-place #f))

(define-syntax-rule (define-state-procedure (sig ...) body ...)
  (define (sig ...) (unless (current-db-connection) (initialize!)) body ...))

(define-state-procedure (declare-file digest path)
  (with-handlers ([exn:fail:sql?
                   (λ (e)
                     (if (eq? (exn:fail:sql-sqlstate e) 'constraint)
                         (raise-user-error 'declare-file
                                           "Cannot redeclare ~a."
                                           path)
                         (raise e)))])
    (query-exec+ "insert into files values (NULL, ?, ?);"
                 (if (path? path)
                     (path->string path)
                     path)
                 digest)
    (:return path ($declare-input digest path))))

(define-state-procedure (declare-package pkginfo)
  (query-exec+ "insert into packages values (NULL, ?, ?, ?, ?, ?);"
               (package-info-provider-name pkginfo)
               (package-info-package-name pkginfo)
               (package-info-edition-name pkginfo)))

(define-state-procedure (declare-dependency input-id derivation-id)
  (query-exec+ "insert into dependencies values (?, ?);"
               input-id
               derivation-id))

(define-state-procedure (declare-derivation package-id digest)
  (query-exec+ "insert into derivations values (NULL, ?, ?);"
               package-id
               digest))

(define-state-procedure (find-latest-package-id query)
  (void))

(define (delete-localstate!)
  (delete-file* (get-localstate-path)))

(define (initialize!)
  (current-db-connection (connect))
  (for ([create-query (in-list create-queries)])
    (query-exec+ create-query))
  (query-exec+ "pragma foreign_keys = on;"))


(define (in-installed-derivations pkginfo)
  (in-query+ "select path from derivations;"))

(define (get-derivation-directory package-id)
  (query-value+ "select path from derivations where package_id=?;" package-id))

(define (mibibytes->bytes mib)
  (inexact->exact (ceiling (* mib 1024 1024))))

(define (make-addressable-file name in est-size)
  (define tmp (build-object-path #"tmp"))
  (dynamic-wind
    void
    (λ ()
      (with-handlers ([values (λ (e) (delete-file* tmp) (raise e))])
        (define bytes-written
          (call-with-output-file tmp #:exists 'truncate/replace
            (λ (to-file)
              (transfer in to-file
                        #:transfer-name name
                        #:max-size (mibibytes->bytes (XIDEN_FETCH_TOTAL_SIZE_MB))
                        #:buffer-size (mibibytes->bytes (XIDEN_FETCH_BUFFER_SIZE_MB))
                        #:timeout-ms (XIDEN_FETCH_TIMEOUT_MS)
                        #:est-size est-size))))
        (define digest (make-digest tmp 'sha384))
        (define path (build-object-path digest))
        (make-directory* (path-only path))
        (rename-file-or-directory tmp path #t)
        (declare-file digest path)))
    (λ () (close-input-port in))))


(define create-file-table #<<EOS
CREATE TABLE IF NOT EXISTS files (
       id INTEGER NOT NULL PRIMARY KEY,
       path TEXT NOT NULL UNIQUE,
       digest BLOB NOT NULL UNIQUE
);
EOS
)


(define create-package-table-query #<<EOS
CREATE TABLE IF NOT EXISTS packages (
       id INTEGER NOT NULL PRIMARY KEY,
       name TEXT NOT NULL,
       provider TEXT NOT NULL,
       edition TEXT NOT NULL
);
EOS
)


(define create-derivations-table-query #<<EOS
CREATE TABLE IF NOT EXISTS derivations (
       id INTEGER NOT NULL PRIMARY KEY,
       package_id INTEGER NOT NULL,
       path TEXT NOT NULL UNIQUE,
       FOREIGN KEY (package_id) REFERENCES packages(id)
);
EOS
)


(define create-revision-name-table-query #<<EOS
CREATE TABLE IF NOT EXISTS revision_names (
       package_id INTEGER NOT NULL,
       revision_number INTEGER NOT NULL,
       revision_name TEXT NOT NULL,
       FOREIGN KEY (package_id) REFERENCES packages(id)
         ON DELETE CASCADE
         ON UPDATE CASCADE
);
EOS
)


(define create-dependency-table-query #<<EOS
CREATE TABLE IF NOT EXISTS dependencies (
       input_id INTEGER NOT NULL,
       derivation_id INTEGER NOT NULL,
       FOREIGN KEY (input_id) REFERENCES inputs(id)
         ON UPDATE RESTRICT
         ON DELETE RESTRICT,
       FOREIGN KEY (derivation_id) REFERENCES derivations(id)
);
EOS
)


(define create-queries
  (list create-file-table
        create-package-table-query
        create-dependency-table-query
        create-derivations-table-query))
