#lang racket/base

(require racket/contract)

(define input-path/c
  (and/c (not/c complete-path?)
         path-string?))

(provide transact
         (contract-out
          [halt-transaction
           (-> any)]
          [get-objects-directory
           (-> complete-path?)]
          [build-object-path
           (-> bytes? complete-path?)]
          [find-latest-package-id
           (-> xiden-query? (or/c #f exact-positive-integer?))]
          [make-addressable-file
           (-> non-empty-string? input-port? (or/c +inf.0 exact-positive-integer?) complete-path?)]
          [make-addressable-directory
           (-> complete-path? (-> complete-path? any) complete-path?)]))


(require racket/match
         "db.rkt"
         "encode.rkt"
         "exn.rkt"
         "file.rkt"
         "format.rkt"
         "integrity.rkt"
         "rc.rkt"
         "message.rkt"
         "path.rkt"
         "port.rkt"
         "printer.rkt"
         "query.rkt"
         "string.rkt"
         "workspace.rkt")

(define+provide-message $init-localstate (path))
(define+provide-message $made-symlink (target-path link-path))
(define+provide-message $deleted-file (path))


(define current-halt-transaction (make-parameter #f))


(define (halt-transaction)
  ((current-halt-transaction)))


(define-syntax-rule (transact body ...)
  (call-with-fs-transaction (λ () body ...)))


(define (get-localstate-path)
  (build-workspace-path "var/xiden/db"))


(define (get-objects-directory)
  (build-workspace-path
   "var/xiden/objects"))


(define (build-object-path digest)
  (build-path (get-objects-directory)
              (encoded-file-name digest)))


(define (connect)
  (define db-path (get-localstate-path))
  (make-directory* (path-only db-path))
  (sqlite3-connect #:database db-path
                   #:mode 'create
                   #:use-place #f))


(define-syntax-rule (define-state-procedure (sig ...) body ...)
  (define (sig ...) (unless (current-db-connection) (initialize!)) body ...))


(define (delete-localstate!)
  (delete-file* (get-localstate-path)))


(define (initialize!)
  (unless (current-db-connection)
    (current-db-connection (connect))
    (for ([create-query (in-list create-queries)])
      (query-exec+ create-query))
    (query-exec+ "pragma foreign_keys = on;")))


(define (in-valid-paths pkginfo)
  (in-query+ "select path from valid_paths;"))


(define (make-addressable-file name in est-size)
  (define tmp (build-object-path #"tmp"))
  (dynamic-wind
    void
    (λ ()
      (with-handlers ([values (λ (e) (delete-file* tmp) (raise e))])
        (call-with-output-file tmp #:exists 'truncate/replace
          (λ (to-file)
            (transfer in to-file
                      #:on-status print-transfer-status
                      #:transfer-name name
                      #:max-size (mibibytes->bytes (XIDEN_FETCH_TOTAL_SIZE_MB))
                      #:buffer-size (mibibytes->bytes (XIDEN_FETCH_BUFFER_SIZE_MB))
                      #:timeout-ms (XIDEN_FETCH_TIMEOUT_MS)
                      #:est-size est-size))))
        (define digest (make-digest tmp 'sha384))
        (define path (build-object-path digest))
        (make-directory* (path-only path))
        (rename-file-or-directory tmp path #t)
        (declare-path (find-relative-path (workspace-directory) path) digest)
        path)
    (λ () (close-input-port in))))


(define (make-addressable-directory containing-dir proc)
  (call-with-temporary-directory
   #:cd? #t #:base containing-dir
   (λ (path)
     (proc path)
     (define digest (make-directory-content-digest path))
     (define dest
       (build-path (path-only path)
                   (encoded-file-name
                    (make-directory-content-digest path))))
     (with-handlers ([exn:fail?
                      (λ (e)
                        (copy-directory/files path dest #:preserve-links? #t)
                        (delete-directory/files path))])
       (rename-file-or-directory path dest #:exists-ok? #t)
       (declare-path dest digest)
       dest))))


(define (make-directory-content-digest path)
  (for/fold ([dig #""])
            ([subpath (in-directory path)]
             #:when (file-exists? subpath))
    (call-with-input-file subpath
      (λ (in) (make-digest (input-port-append (open-input-bytes dig) in)
                           'sha384)))))

(define (print-transfer-status m)
  (write-message m
   (message-formatter
    [($transfer-progress name bytes-read max-size timestamp)
     (format "~a%" (~r (* 100 (/ bytes-read max-size)) #:precision 0))]
    [($transfer-small-budget name)
     (format "Cannot transfer ~s. The configured budget is too small."
             name)]
    [($transfer-over-budget name size)
     (format "Halting transfer ~s. The transfer produced more than the estimated ~a bytes."
             name
             size)]
    [($transfer-timeout name bytes-read)
     (format "Halting transfer ~s after ~a bytes. Read timed out."
             name bytes-read)])))

(define-state-procedure (path-declared? path)
  (eq? 1 (query-value+ "select exists (select 1 from valid_paths where path=?);" (~a path))))

(define-state-procedure (rollback-fs-transaction)
  (with-handlers ([exn:fail:sql? void]) (query-exec+ "rollback transaction;"))
  (for ([path (in-directory (get-objects-directory))])
    (unless (path-declared? path)
      (delete-directory/files #:must-exist? #f path))))


(define-state-procedure (call-with-fs-transaction act!)
  (if (eq? current-halt-transaction void) ; Prevents nesting transactions
      (begin (query-exec+ "begin exclusive transaction;")
             (call/cc
              (λ (k)
                (parameterize ([current-halt-transaction
                                (λ () (rollback-fs-transaction) (k (void)))])
                  (with-handlers ([values (λ (e) (rollback-fs-transaction) (raise e))])
                    (act!)
                    (query-exec+ "commit transaction;")
                    (void))))))
      (begin (act!)
             (void))))


(define-state-procedure (declare-path path digest)
  (unless (path-declared? path)
    (query-exec+ "insert into valid_paths values (NULL, ?, ?);"
                 (~a path)
                 digest)))


(define-state-procedure (declare-link target link-path)
  (match-define (vector target-id digest) (query-row+ "select id, digest from valid_paths where path=?;" target))
  (declare-path link-path digest)
  (define link-id (query-value+ "select id from valid_paths where path=?;" link-path))
  (query-exec+ "insert into links values (NULL, ?, ?);"
               link-id
               target-id))


(define-state-procedure (find-latest-package-id query)
  (void))


(define create-valid-path-table-query #<<EOS
CREATE TABLE IF NOT EXISTS valid_paths (
       id INTEGER PRIMARY KEY UNIQUE NOT NULL,
       path TEXT UNIQUE NOT NULL,
       digest BLOB
);
EOS
)


(define create-revision-name-table-query #<<EOS
CREATE TABLE IF NOT EXISTS revision_names (
       edition_id INTEGER NOT NULL,
       revision_number INTEGER NOT NULL,
       revision_name TEXT NOT NULL,
       FOREIGN KEY (package_id) REFERENCES packages(id)
         ON DELETE CASCADE
         ON UPDATE CASCADE
);
EOS
)


(define create-dependency-table-query #<<EOS
CREATE TABLE IF NOT EXISTS links (
       link_id INTEGER NOT NULL PRIMARY KEY,
       target_path_id INTEGER NOT NULL,
       link_path_id INTEGER NOT NULL,
       FOREIGN KEY (target_path_id) REFERENCES valid_paths(id)
         ON UPDATE RESTRICT
         ON DELETE RESTRICT,
       FOREIGN KEY (link_path_id) REFERENCES valid_paths(id)
         ON UPDATE RESTRICT
         ON DELETE RESTRICT
);
EOS
)

(define create-queries
  (list create-valid-path-table-query))
