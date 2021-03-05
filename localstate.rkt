#lang racket/base

; Provide interface between program and local database. No SQL should
; appear anywhere else, and this module should concern itself with
; keeping filesystem and database consistent.
;
; DANGER: Some queries in this section are built using string
; concatenation because the db module can only parameterize SQL
; literals, not column or relation names.
; https://docs.racket-lang.org/db/using-db.html?q=db#%28part._dbsec-sql-injection%29
;
; Any procedure that can only ever use string concatenation to be
; useful are marked with "/unsafe". It should go without saying, but
; never include user input in any string processing here, and
; never provide */unsafe procedures from this module.

(require racket/contract)

; Provided ids include relation structs. See use of define-relation.
(provide (struct-out record)
         (contract-out
          [xiden-collect-garbage
           (-> exact-nonnegative-integer?)]
          [in-all-installed
           (-> sequence?)]
          [declare-output
           (-> non-empty-string?
               non-empty-string?
               non-empty-string?
               exact-nonnegative-integer?
               (listof non-empty-string?)
               string?
               path-record?
               output-record?)]
          [find-path-record
           (-> any/c (or/c path-record? #f))]
          [find-package-query
           (-> exact-positive-integer?
               string?
               (or/c parsed-package-query? #f))]
          [call-with-reused-output
           (-> package-query-variant?
               string?
               (-> (or/c #f exn? output-record?) any)
               any)]
          [in-xiden-outputs
           (-> package-query-variant?
               string?
               (sequence/c output-record?))]
          [in-path-links
           (-> path-record? (sequence/c path-record?))]
          [find-exactly-one
           (->* (record?) (procedure?) (or/c #f record?))]
          [start-transaction!
           (-> (values (-> void?) (-> void?)))]
          [build-object-path
           (->* () #:rest (listof path-string?) complete-path?)]
          [build-addressable-path
           (-> bytes? complete-path?)]
          [in-issued-links
           (-> (sequence/c path-string? path-string?))]
          [in-xiden-objects
           (-> package-query-variant?
               string?
               (sequence/c path-string?
                           exact-positive-integer?
                           revision-number?
                           exact-positive-integer?
                           path-string?))]
          [make-addressable-file
           (->* (non-empty-string?
                 input-port?
                 (or/c +inf.0 exact-positive-integer?)
                 #:on-status (-> $message? any)
                 #:max-size (or/c +inf.0 exact-positive-integer?)
                 #:buffer-size exact-positive-integer?
                 #:timeout-ms (>=/c 0))
                (#:cache-key (or/c bytes? #f))
               path-record?)]
          [make-addressable-directory
           (-> (non-empty-listof input-port?)
               path-record?)]
          [delete-record
           (-> record? void?)]
          [make-addressable-link
           (-> path-record? path-string? path-record?)]))


(require (for-syntax racket/base
                     racket/string
                     racket/syntax
                     syntax/stx)
         racket/function
         racket/generic
         racket/list
         racket/match
         racket/sequence
         racket/stream
         racket/vector
         db
         "codec.rkt"
         "exn.rkt"
         "file.rkt"
         "format.rkt"
         "integrity.rkt"
         "message.rkt"
         "path.rkt"
         "port.rkt"
         "query.rkt"
         "string.rkt"
         "version.rkt"
         "workspace.rkt")


(define+provide-message $finished-collecting-garbage (bytes-recovered))

;----------------------------------------------------------------------------------
; Relevant Paths

(define current-get-localstate-path ; Is a parameter for testing reasons.
  (make-parameter (λ () (build-workspace-path "var/xiden/db"))))

(define build-object-path
  (make-workspace-path-builder "var/xiden/objects"))

(define (build-addressable-path digest)
  (build-object-path (encoded-file-name digest)))


;------------------------------------------------------------------------------
; Use Generics/Macros to map relations and records to Racket struct
; types.  To avoid the disadvantages of ORMs, nothing here will
; prevent direct use of SQL. The objective is only to control how much
; SQL leaks over the rest of the code.

(define-generics relatable
  (gen-relation    relatable)
  (gen-columns     relatable)
  (gen-constructor relatable)
  (gen-save        relatable))

(struct relation (name fields)
  #:transparent
  #:methods gen:relatable
  [(define (gen-relation r) r)
   (define (gen-constructor r) relation)
   (define (gen-columns r)
     (map (λ (x) (car (string-split x " ")))
          (relation-fields r)))])

(struct record (id) #:transparent)

(define-syntax (define-relation stx)
  (syntax-case stx ()
    [(_ relation-id (fields ...) clauses ...)
     (let* ([relation-name (symbol->string (syntax-e #'relation-id))]
            [relation_name (string-replace relation-name "-" "_")])
       (with-syntax ([relation_name-patt relation_name]
                     [record-id (format-id #'relation-id "~a-record"
                                           (substring relation-name 0
                                                      (sub1 (string-length relation-name))))])
         #'(begin (define relation-id (relation relation_name-patt
                                                (list "id INTEGER PRIMARY KEY UNIQUE NOT NULL"
                                                      clauses ...)))
                  (provide (struct-out record-id))
                  (struct record-id record (fields ...)
                    #:transparent
                    #:methods gen:relatable
                    [(define/generic relation-cols gen-columns)
                     (define (gen-relation r)
                       relation-id)
                     (define (gen-constructor r)
                       record-id)
                     (define (gen-columns r)
                       (relation-cols relation-id))
                     (define (gen-save r)
                       (struct-copy record-id r
                                    [id #:parent record (save r)]))]))))]))


;------------------------------------------------------------------------------
; Relation Definitions

(define-relation editions (name package-id)
  "name TEXT NOT NULL"
  "package_id INTEGER NOT NULL"
  "FOREIGN KEY (package_id) REFERENCES packages(id)")

(define-relation outputs (revision-id path-id name)
  "revision_id INTEGER NOT NULL"
  "path_id INTEGER NOT NULL"
  "name TEXT NOT NULL"
  "FOREIGN KEY (revision_id) REFERENCES revisions(id) ON DELETE CASCADE ON UPDATE CASCADE"
  "FOREIGN KEY (path_id)     REFERENCES paths(id) ON DELETE CASCADE ON UPDATE CASCADE")

(define-relation packages (name provider-id)
  "name TEXT NOT NULL"
  "provider_id INTEGER NOT NULL"
  "FOREIGN KEY (provider_id) REFERENCES providers(id)")

(define-relation paths (path digest target-id)
  "path TEXT UNIQUE NOT NULL"
  "digest BLOB"
  "target_id INTEGER"
  "FOREIGN KEY (target_id) REFERENCES paths(id) ON DELETE RESTRICT ON UPDATE CASCADE")

(define-relation providers (provider-name)
  "name TEXT NOT NULL")

(define-relation revisions (number edition-id)
  "number INTEGER NOT NULL"
  "edition_id INTEGER NOT NULL"
  "FOREIGN KEY (edition_id) REFERENCES editions(id)")

(define-relation revision-names (revision-name revision-id)
  "name TEXT NOT NULL"
  "revision_id INTEGER NOT NULL"
  "FOREIGN KEY (revision_id) REFERENCES revisions(id)")

(define-relation path-keys (key path-id)
  "key BLOB UNIQUE NOT NULL"
  "path_id INTEGER NOT NULL"
  "FOREIGN KEY (path_id) REFERENCES paths(id) ON DELETE CASCADE ON UPDATE CASCADE")

(define ALL_RELATIONS
  (list editions
        outputs
        packages
        paths
        path-keys
        providers
        revisions
        revision-names))


;----------------------------------------------------------------------------------
; Define DB bindings such that any attempt to execute a query lazily creates
; all needed resources.

(define current-db-connection (make-parameter #f))

(define (with-lazy-initialization f)
  (make-keyword-procedure
   (λ (k a . formals)
     (connect-if-needed!)
     (with-handlers ([exn:fail:sql?
                      (λ (e) ; Create used tables when they are missing
                        (if (regexp-match? #rx"no such table"
                                           (cdr (assoc 'message (exn:fail:sql-info e))))
                            (begin (map create ALL_RELATIONS)
                                   (keyword-apply f k a (current-db-connection)
                                                  formals))
                            (raise e)))])
       (keyword-apply f k a (current-db-connection)
                      formals)))))

(define (connect-if-needed!)
  (unless (current-db-connection)
    (define db-path ((current-get-localstate-path)))
    (make-directory* (path-only db-path))
    (define conn
      (sqlite3-connect #:database db-path
                       #:mode 'create
                       #:use-place #f))
    (current-db-connection conn)
    (query-exec+ "pragma foreign_keys = on;")))


(define query-exec+        (with-lazy-initialization query-exec))
(define query-rows+        (with-lazy-initialization query-rows))
(define query-list+        (with-lazy-initialization query-list))
(define query-row+         (with-lazy-initialization query-row))
(define query-maybe-row+   (with-lazy-initialization query-maybe-row))
(define query-value+       (with-lazy-initialization query-value))
(define query-maybe-value+ (with-lazy-initialization query-maybe-value))
(define in-query+          (with-lazy-initialization in-query))
(define prepare+           (with-lazy-initialization prepare))


;----------------------------------------------------------------------------------
; Generic CRUD procedures

(define (create has-relation)
  (define relation-inst (gen-relation has-relation))
  (query-exec+
   (format "CREATE TABLE IF NOT EXISTS ~a (\n~a\n);"
           (relation-name relation-inst)
           (string-join (map (λ (i) (format "    ~a" i))
                             (relation-fields relation-inst))
                        ",\n"))))


(define (load-by-id relation-inst record-ctor id)
  (apply record-ctor (vector->list
                      (query-row+ (~a "select * from " (relation-name relation-inst)
                                      " where id=?;")
                                  id))))


(define (load-by-record record-inst)
  (load-by-id (gen-relation record-inst)
              (gen-constructor record-inst)
              (record-id record-inst)))


(define (save record-inst)
  (define vals (vector-drop (struct->vector record-inst) 1))
  (define existing-or-#f (find-exactly-one record-inst))
  (define id (or (vector-ref vals 0)
                 (if existing-or-#f
                     (record-id existing-or-#f)
                     sql-null)))

  (define insert? (or (not id) (sql-null? id)))

  (define sql
    (~a (if insert? "insert" "replace")
        " into " (relation-name (gen-relation record-inst))
        " values ("
        (string-join (build-list (vector-length vals) (const "?")) ",")
        ");"))

  (apply query-exec+ sql (cons id (cdr (vector->list vals))))
  (if insert?
      (query-value+ "SELECT last_insert_rowid();")
      id))


(define (delete-record record-inst [relation (gen-relation record-inst)])
  (query-exec+
   (~a "delete from " (relation-name relation) " where id=?;")
   (record-id record-inst)))


(define (error-code-equal? code e)
  (equal? code (cdr (assoc 'errcode (exn:fail:sql-info e)))))


;----------------------------------------------------------------------------------
; Garbage Collector
;
; 1. Delete all invalid records of links.
; 2. Delete any record of objects with no incoming links.
; 3. If database changed in step 2, go to step 1.
; 4. Delete all object files with no corresponding record
;
; TODO: There's a bug where not everything is collected in one
; pass. The second pass always gets it. Use `extra?' to run it twice
; until the root cause gets fixed.

(define (xiden-collect-garbage)
  (parameterize ([current-directory (workspace-directory)]
                 [current-security-guard (make-gc-security-guard)])
    (if (directory-exists? (current-directory))
        (let loop ([bytes-recovered 0] [extra? #t])
          (forget-missing-links!)
          (forget-unlinked-paths!)
          (define bytes-recovered* (+ bytes-recovered (delete-unreferenced-objects!)))
          (if (or extra? (> bytes-recovered* bytes-recovered))
              (loop bytes-recovered* #f)
              bytes-recovered*))
        0)))

(define (make-gc-security-guard)
  (make-security-guard (current-security-guard)
                       (λ (sym path-or-#f ops)
                         (define allowed?
                           (or (eq? sym 'sqlite3-connect)
                               (not path-or-#f)
                               (if (member 'delete ops)
                                   (path-prefix? (simple-form-path path-or-#f) (build-object-path))
                                   (not (or (member 'write ops)
                                            (member 'execute ops))))))

                         (unless allowed?
                           (raise-user-error 'gc-security
                                             "Blocked ~a on ~a: ~e"
                                             sym
                                             path-or-#f
                                             ops)))
                       (λ (sym host port cs)
                         (error 'gc-security
                                "Blocked ~a on ~a:~a (~e)" sym host port cs))
                       #f))



(define (in-unreferenced-paths)
  (define referenced-paths
    (~a "select target_id from "
        (relation-name paths)
        " where target_id is not NULL"))
  (in-query+
   (~a "select P.id,P.path from " (relation-name paths) " as P "
       " where target_id is NULL and "
       " id not in (" referenced-paths ");")))


(define (forget-missing-links!)
  (define links (~a "select id,path from " (relation-name paths) " where target_id is not NULL;"))
  (for ([(id path) (in-query+ links)])
    (unless (link-exists? path)
      (delete-record (record id) paths))))


(define (forget-unlinked-paths!)
  (for ([(id path) (in-unreferenced-paths)])
    (delete-record (record id) paths)))


; Not atomic, but can be used again unless the database itself is corrupted.
(define (delete-unreferenced-objects! [dir (build-object-path)])
  (for/sum ([path (in-list (directory-list dir #:build? #t))])
    (if (find-path-record (find-relative-path (workspace-directory) path))
        0
        (cond [(directory-exists? path)
               (define recovered (delete-unreferenced-objects! path))
               (delete-directory/files path)
               recovered]
              [(file-exists? path)
               (define size (file-size path))
               (delete-file path)
               size]
              [(link-exists? path)
               (delete-file path)
               ; This is not a correct size, but this tells the garbage collector
               ; that something was deleted. I'm assuming that users run the garbage
               ; collector to save enough bytes that the error would become negligible.
               1]))))



;------------------------------------------------------------------
; Record-Based Queries
;
; search-by-record constructs a SELECT query based on "holes" in a
; given record. In that sense, (search-by-record (customer #f "John"
; "Doe" #f #f)) returns a sequence of John Does in the database.
;
; The search goes by exact match, so construct your own SELECT
; if different comparisons are necessary.


(define (search-by-record record-inst [ctor (gen-constructor record-inst)])
  (define available-values (vector->list (vector-drop (struct->vector record-inst) 1)))
  (define query-args (infer-select-query record-inst available-values))
  (if (null? query-args)
      (in-value record-inst)
      (sequence-map (λ from-db (apply ctor (fill-holes null (reverse from-db) available-values)))
                    (apply in-query+ query-args))))


; Take the name literally! This returns #f if a query returns more than one record.
(define (find-exactly-one record-inst [ctor (gen-constructor record-inst)])
  (define seq (search-by-record record-inst ctor))
  (and (with-handlers ([values (const #t)]) (sequence-ref seq 1) #f)
       (with-handlers ([values (const #f)]) (sequence-ref seq 0))))


(define (infer-select-clauses available-values cols)
  (for/fold ([wip-requested-columns null]
             [wip-conditions null]
             [wip-params null])
            ([val (in-list available-values)]
             [col (in-list cols)])
    (if (hole? val)
        (values (cons col wip-requested-columns)
                wip-conditions
                wip-params)
        (values wip-requested-columns
                (cons (format "~a=?" col) wip-conditions)
                (cons val wip-params)))))


(define (infer-select-query record-inst available-values)
  (define cols (gen-columns record-inst))
  (define-values (fields conditions params) (infer-select-clauses available-values cols))
  (if (null? fields)
      null
      (cons
       (~a "select "
           (string-join fields ",")
           " from "
           (relation-name (gen-relation record-inst))
           (if (null? conditions)
               ""
               (~a " where "
                   (string-join conditions " and ")))
           ";")
       params)))


(define (hole? v)
  (or (not v)
      (sql-null? v)))


(define (fill-holes built-args from-db from-template)
  (if (null? from-template)
      (reverse built-args)
      (let ([v (car from-template)])
        (if (hole? v)
            (if (null? from-db)
                (error 'fill-holes "Ran out of elements to replace #f values in `from-template`")
                (fill-holes (cons (car from-db) built-args)
                            (cdr from-db)
                            (cdr from-template)))
            (fill-holes (cons v built-args)
                        from-db
                        (cdr from-template))))))




;----------------------------------------------------------------------------------
; Transaction mechanism: Use DBMS transaction, such that a rollback
; leaves a discrepency between the DB and the filesystem.  To rollback
; the filesystem, delete whatever is not declared in the DB.


(define (rollback-transaction!)
  (with-handlers ([exn:fail:sql?
                   (λ (e)
                     (unless (regexp-match? #rx"no transaction is active" (exn-message e))
                       (raise e)))])
    (query-exec+ "rollback transaction;"))
  (delete-unreferenced-objects!)
  (void))


(define (start-transaction!)
  (with-handlers
    ([exn:fail:sql?
      (λ (e)
        (define error-info (exn:fail:sql-info e))
        (unless (regexp-match? #rx"transaction within a transaction"
                               (cdr (assoc 'message error-info)))
            (raise e)))])
    (query-exec+ "begin exclusive transaction;"))
  (values end-transaction!
          rollback-transaction!))


(define (end-transaction!)
  (query-exec+ "commit transaction;")
  (void))


;----------------------------------------------------------------------------------
; Hybrid Database and File I/O
;
; These procedures control file and database I/O, such that each
; written file comes with a declaration in the database that the path
; exists and is valid.
;
; For a given path P:
;
;  - If DB declares P
;    * P exists: DB integrity is fine.
;    * P does not exist: DB is corrupt. Not recoverable.
;  - If DB does not declare P
;    * P exists and digest matches: Filesystem integrity is fine.
;    * P does not exist, or digest does not match: FS is corrupt. Delete P to recover.


(define (make-addressable-file #:on-status on-status
                               #:max-size max-size
                               #:buffer-size buffer-size
                               #:timeout-ms timeout-ms
                               #:cache-key [cache-key #f]
                               name
                               in
                               est-size)
  (or (and cache-key (look-up-path cache-key))
      (let ([tmp (build-addressable-path #"tmp")])
        (dynamic-wind
          void
          (λ ()
            (with-handlers ([values (λ (e) (delete-file* tmp) (raise e))])
              (make-directory* (path-only tmp))
              (call-with-output-file tmp #:exists 'truncate/replace
                (λ (to-file)
                  (transfer in to-file
                            #:on-status on-status
                            #:transfer-name name
                            #:max-size max-size
                            #:buffer-size buffer-size
                            #:timeout-ms timeout-ms
                            #:est-size est-size))))
            (define digest (make-digest tmp 'sha384))
            (define path (build-addressable-path digest))
            (make-directory* (path-only path))
            (rename-file-or-directory tmp path #t)
            (define path-record (declare-path (find-relative-path (workspace-directory) path) digest))
            (when cache-key
              (gen-save (path-key-record cache-key (record-id path-record))))

            path-record)
          (λ () (close-input-port in))))))


(define (make-addressable-directory digest-ports)
  (define digest (make-digest (apply input-port-append #t digest-ports) 'sha384))
  (define path (build-addressable-path digest))
  (make-directory* path)
  (declare-path (find-relative-path (workspace-directory) path)
                digest))


(define (make-addressable-link target-path-record user-link-path)
  ; If the link to create is inside the workspace, make it use a relative
  ; path. This allows the user to move the workspace directory without
  ; breaking the links inside.
  (define link-in-workspace? (path-in-workspace? user-link-path))

  (when (or (link-exists? user-link-path)
            (directory-exists? user-link-path)
            (file-exists? user-link-path))
    (raise-user-error (format "Cannot make link at ~a. Something already exists at that path."
                              user-link-path)))

  (define to-path
    (make-link-content-path #:link-in-workspace? link-in-workspace?
                            target-path-record
                            user-link-path))

  (define link-path
    (make-link-path #:link-in-workspace? link-in-workspace?
                    user-link-path))

  (define link-record
    (declare-link link-path target-path-record))

  (make-directory* (path-only (simple-form-path user-link-path)))
  (make-file-or-directory-link to-path user-link-path)

  link-record)


(define (declare-link link-path target-path-record)
  (define normalized (normalize-path-for-db link-path))

  ; A redundant record is not cause to halt the program. If anything,
  ; it's good that it exists after recreating a missing link.
  (with-handlers ([exn:fail:sql?
                   (λ (e)
                     (unless (error-code-equal? 2067 e)
                       (raise e))
                     (find-exactly-one (make-file-path-record normalized #f)))])
    (gen-save (make-link-path-record normalized
                                     (record-id target-path-record)))))


(define (make-link-path #:link-in-workspace? link-in-workspace? link-path)
  (let ([simple (simple-form-path link-path)])
    (if link-in-workspace?
        (find-relative-path (workspace-directory) simple)
        simple)))


(define (make-link-content-path  #:link-in-workspace? link-in-workspace? target-path-record link-path)
  (if link-in-workspace?
      (find-relative-path (path-only (simple-form-path link-path))
                          (build-workspace-path (path-record-path target-path-record)))
      ; This breaks if the path record has a complete path.
      ; It assumes that the link target is always in the workspace.
      (simple-form-path (build-workspace-path (path-record-path target-path-record)))))



(define (make-file-path-record path digest)
  (path-record sql-null
               (normalize-path-for-db path)
               digest
               sql-null))


(define (make-link-path-record path path-id)
  (path-record sql-null
               path
               sql-null
               path-id))


(define (find-path-record variant)
  (define search-rec
    (cond [(exact-positive-integer? variant)
           (path-record variant
                        #f
                        #f
                        #f)]
          [(path-string? variant)
           (path-record #f
                        (normalize-path-for-db variant)
                        #f
                        #f)]
          [(bytes? variant)
           (path-record #f
                        #f
                        variant
                        #f)]
          [else #f]))
  (and search-rec
       (find-exactly-one search-rec)))


; It's possible for make-addressable-file to try creating two path
; records of the same name from different sources because it uses
; a content-addressing scheme. Unique constraint violations are
; therefore handled by returning existing records.
(define (declare-path unnormalized-path digest)
  (let ([rec (make-file-path-record unnormalized-path digest)])
    (with-handlers ([exn:fail:sql?
                     (λ (e)
                       (if (error-code-equal? 2067 e)
                           (find-exactly-one (path-record #f (path-record-path rec) #f #f))
                           (raise e)))])
      (gen-save rec))))

(define (in-path-links path-id)
  (search-by-record (path-record #f #f #f path-id)))

(define (in-issued-links)
  (in-query+ (~a "select L.path, P.path from " (relation-name paths) " as P "
                 "inner join " (relation-name paths) " as L "
                 "on L.target_id = P.id;")))

(define (look-up-path key)
  (define r (find-exactly-one (path-key-record #f key #f)))
  (and r
       (find-exactly-one (path-record (path-key-record-path-id r) #f #f #f))))


(define (normalize-path-for-db path)
  (if (string? path)
      path
      (path->string path)))



;----------------------------------------------------------------------------------
; Package output includes discovery information (version, author,
; name) and a path record. Define procedures to help users search for
; and review their installed objects.

(define (declare-output provider-name package-name edition-name
                        revision-number revision-names output-name output-path-record)

  (define (insert-if-new r)
    (define existing (find-exactly-one r))
    (if existing
        (record-id existing)
        (save r)))

  (define provider-id  (insert-if-new (provider-record #f provider-name)))
  (define package-id   (insert-if-new (package-record  #f package-name provider-id)))
  (define edition-id   (insert-if-new (edition-record  #f edition-name package-id)))

  (define revision-rec (revision-record #f revision-number edition-id))
  (define existing-revision (find-exactly-one revision-rec))
  (define revision-id
    (if existing-revision
        (record-id existing-revision)
        (save revision-rec)))

  (unless existing-revision
    (for/list ([name (in-list revision-names)])
      (save (revision-name-record #f name revision-id))))

  (gen-save (output-record #f
                           revision-id
                           (record-id output-path-record)
                           output-name)))


(define (find-revision-number v edition-id)
  (cond [(equal? v "")
         (query-maybe-value+
          (~a "select number from "
              (relation-name revisions)
              " order by number desc limit 1;"))]
        [(revision-number? v) v]
        [(revision-number-string? v) (string->number v)]
        [else (query-maybe-value+
               (~a "select R.number from "
                   (relation-name revisions) " as R "
                   " inner join "
                   (relation-name revision-names) " as N "
                   " on R.id = N.revision_id "
                   " where R.edition_id=? and "
                   " N.name=? "
                   " limit 1;")
               edition-id v)]))


(define (in-all-installed)
  (in-query+
   (~a "select U.id, U.name, K.id, K.name, E.id, E.name, R.id, R.number, O.id, O.name, P.id, P.path from "
       (relation-name paths) " as P"
       " inner join " (relation-name outputs) " as O on O.path_id = P.id"
       " inner join " (relation-name revisions) " as R on R.id = O.revision_id "
       " inner join " (relation-name editions) " as E on E.id = R.edition_id"
       " inner join " (relation-name packages) " as K on K.id = E.package_id"
       " inner join " (relation-name providers) " as U on U.id = K.provider_id")))



(define (in-xiden-objects query-variant output-name)
  (define query (coerce-parsed-package-query query-variant))
  (match-define
    (parsed-package-query
     provider-name
     package-name
     edition-name
     revision-min
     revision-max
     interval-bounds)
    query)

  (call/cc
   (λ (return)
     (define (fail . _) (return empty-sequence))

     ; A failure to match some queries
     ; implies that no outputs will match.
     (define (q arg)
       (define rec-or-#f (find-exactly-one arg))
       (if rec-or-#f
           (record-id rec-or-#f)
           (fail)))

     (define provider-id (q (provider-record #f provider-name)))
     (define package-id  (q (package-record  #f package-name provider-id)))
     (define edition-id  (q (edition-record  #f edition-name package-id)))

     (define-values (lo hi)
       (resolve-revision-interval
        query
        (λ (_ rev)
          (string->number (~a (or (find-revision-number rev edition-id)
                                  (fail)))))))

     (when (< hi lo)
       (fail))

     (define sql
       (~a "select O.name, R.id, R.number, P.id, P.path from "
           (relation-name paths) " as P"
           " inner join " (relation-name outputs) " as O"
           " on O.path_id = P.id"
           " inner join " (relation-name revisions) " as R"
           " on R.id = O.revision_id "
           " where R.edition_id=? and"
           " R.number >= ? and"
           " R.number <= ?"
           " order by R.number desc;"))

     (define params
       (list edition-id lo hi))

     (apply in-query+ sql params))))


(define (in-xiden-outputs query-variant output-name)
  (sequence-map (λ (output-name revid revno pathid path) (find-exactly-one (output-record #f revid #f #f)))
                (in-xiden-objects query-variant output-name)))


(define (find-package-query output-name revision-id)
  (match-define (vector provider-name package-name edition-name revision-number)
    (query-row+ (~a "select P.name, K.name, E.name, R.number"
                    " from "       (relation-name providers) " as P"
                    " inner join " (relation-name packages)  " as K on P.id = K.provider_id"
                    " inner join " (relation-name editions)  " as E on K.id = E.package_id"
                    " inner join " (relation-name revisions) " as R on E.id = R.edition_id"
                    " where R.id = ?")
                revision-id))
  (parsed-package-query provider-name
                        package-name
                        edition-name
                        (~a revision-number)
                        (~a revision-number)
                        "ii"
                        output-name))


(define (call-with-reused-output query output-name continue)
  (continue
   (call/cc
    (λ (k)
      (with-handlers ([(λ _ #t)
                       (λ (e)
                         ; Consider early termination of a sequence as benign. Just
                         ; report that as no record found.
                         (k (if (regexp-match? #rx"ended before index" (exn-message e))
                                #f
                                e)))])
        (define-values (_ rev-id rev-no path-id path)
          (sequence-ref (in-xiden-objects query output-name) 0))
        (find-exactly-one (output-record #f rev-id #f output-name)))))))


(module+ test
  (require rackunit)

  (define (run-db-test msg p)
    (test-case msg
      (define t (make-temporary-file))
      (dynamic-wind void
                    (λ ()
                      (parameterize ([current-db-connection #f]
                                     [current-get-localstate-path (const t)])
                        (p)))
                    (λ () (delete-file t)))))

  (define-syntax-rule (test-db msg body ...)
    (run-db-test msg (λ () body ...)))

  (test-db "Declare paths"
    (declare-path "a/b/c" #"digest")
    (define expected (path-record 1 "a/b/c" #"digest" sql-null))
    (check-equal? (find-exactly-one (path-record #f "a/b/c" #f #f))
                  expected)

    (test-case "Return existing records when trying to save a duplicate path"
      (check-equal? (declare-path "a/b/c" #"different")
                    expected))

    (test-case "Declare a path key"
      (gen-save (path-key-record #f #"name" 1))
      (define actual (look-up-path #"name"))
      (check-equal? actual expected)
      (test-equal? "Use path key with make-addressable-file"
                   (make-addressable-file #:cache-key #"name"
                                          #:on-status void
                                          #:buffer-size 0
                                          #:timeout-ms 0
                                          #:max-size 0
                                          ""
                                          (open-input-bytes #"")
                                          1)
                   expected))

    (test-false "Find no path when using undefined key"
                (look-up-path #"nothing")))


  (test-db "Declare an output"
    (define pathrec
      (declare-path "outpath" #"abcd"))

    (define revision-names
      '("prod" "live"))

    ; We can assume the ID is 1 because this is always a fresh database
    (define edition-id 1)

    (define outrec
      (declare-output "example.com"
                      "widget"
                      "default"
                      12
                      revision-names
                      "lib"
                      pathrec))

    (check-equal? outrec (output-record 1 1 1 "lib"))

    (for ([name (in-list revision-names)])
      (test-equal? (format "Resolve revision name ~a" name)
                   (find-revision-number name edition-id)
                   12)))


  (test-db "Query installed outputs"
    (define (mock-install query revision-number revision-names)
      (declare-output (parsed-package-query-provider-name query)
                      (parsed-package-query-package-name query)
                      (parsed-package-query-edition-name query)
                      revision-number
                      revision-names
                      (~a "out" revision-number)
                      (declare-path (~a "path" revision-number)
                                    #"abcd")))

    (define (mock-install-line qs N)
      (define query (coerce-parsed-package-query qs))
      (for ([i (in-range N)])
        (mock-install query i (list (format "rev-~a" i)
                                    (format "alt-~a" i)))))

    (mock-install-line "a.example.com:widget:default" 10)

    (define actual-results
      (sequence->list
       (in-values-sequence
        (in-xiden-objects "a.example.com:widget:default:rev-3:7:ei" "lib"))))

    (check-equal? (map (match-lambda [(list o _ rev-n _ path) (list rev-n o path)]) actual-results)
                  (build-list 4
                              (λ (i [n (- 7 i)])
                                (list n (~a "out" n) (~a "path" n))))))


  (test-case "Infer clauses for a SELECT query to find missing information"
    (call-with-values (λ () (infer-select-clauses '(192 #f "mark") '("id" "foo" "bar")))
                      (λ (cols conditions params)
                        (check-equal? cols '("foo"))
                        (check-equal? conditions '("bar=?" "id=?"))
                        (check-equal? params '("mark" 192)))))

  (test-case "Fill holes in available values from a record"
    (check-equal? (fill-holes null '(1 2 3) '(a #f 8 #f b #f))
                  '(a 1 8 2 b 3))
    (check-equal? (fill-holes null '(1 2 3) '(#f #f #f))
                  '(1 2 3))
    (test-exn "Raise an error if there are more holes than data"
              exn:fail?
              (λ () (fill-holes null '(1 2 3) '(#f #f #f #f))))))
