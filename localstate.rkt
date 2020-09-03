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
(module+ test (require rackunit))

(provide transact
         (contract-out
          [halt-transaction
           (-> any)]
          [get-objects-directory
           (-> complete-path?)]
          [build-object-path
           (-> bytes? complete-path?)]
          [in-valid-derivations
           (-> xiden-query? sequence?)]
          [make-addressable-file
           (-> non-empty-string? input-port? (or/c +inf.0 exact-positive-integer?) complete-path?)]
          [make-addressable-directory
           (-> complete-path? (-> complete-path? any) complete-path?)]))


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
; Entity definitions

(define-relation paths (path digest)
  "path TEXT UNIQUE NOT NULL"
  "digest BLOB")

(define-relation providers (provider-name)
  "name TEXT NOT NULL")

(define-relation packages (provider-id)
  "name TEXT NOT NULL"
  "provider_id INTEGER NOT NULL"
  "FOREIGN KEY (provider_id) REFERENCES providers(id)")

(define-relation editions (package-id)
  "name TEXT NOT NULL"
  "package_id INTEGER NOT NULL"
  "FOREIGN KEY (package_id) REFERENCES packages(id)")

(define-relation revisions (revision-number edition-id)
  "number INTEGER NOT NULL"
  "edition_id INTEGER NOT NULL"
  "FOREIGN KEY (edition_id) REFERENCES editions(id)")

(define-relation revision-names (revision-id revision-name)
  "name TEXT NOT NULL"
  "revision_id INTEGER NOT NULL"
  "FOREIGN KEY (revision_id) REFERENCES revisions(id)")

(define-relation links (link-id target-path-id link-path-id)
  "link_id INTEGER NOT NULL PRIMARY KEY"
  "target_path_id INTEGER NOT NULL"
  "link_path_id INTEGER NOT NULL"
  "FOREIGN KEY (target_path_id) REFERENCES paths(id) ON DELETE RESTRICT ON UPDATE RESTRICT"
  "FOREIGN KEY (link_path_id)   REFERENCES paths(id) ON DELETE RESTRICT ON UPDATE RESTRICT")

(define-relation derivations (revision-id path-id)
  "id INTEGER NOT NULL PRIMARY KEY"
  "revision_id INTEGER NOT NULL"
  "path_id INTEGER NOT NULL"
  "FOREIGN KEY (revision_id) REFERENCES revisions(id) ON DELETE CASCADE ON UPDATE CASCADE"
  "FOREIGN KEY (path_id)     REFERENCES paths(id) ON DELETE CASCADE ON UPDATE CASCADE")



;----------------------------------------------------------------------------------
; DB procedures are normal procedures that lazily prepare a connection
; and/or create missing tables.

(define-syntax-rule (define-db-procedure (id sig ...) body ...)
  (define id (make-db-procedure (λ (sig ...)  body ...))))


(define (connect)
  (define db-path (get-localstate-path))
  (make-directory* (path-only db-path))
  (sqlite3-connect #:database db-path
                   #:mode 'create
                   #:use-place #f))


; This is the Big Red Button. Only press when prototyping in the REPL.
(define (delete-localstate!)
  (delete-file* (get-localstate-path))
  (current-db-connection #f))


(define (make-db-procedure f)
  (λ formals
    (with-handlers ([exn:fail:sql?
                     (λ (e) ; Create table when it's missing
                       (if (regexp-match? #rx"no such table"
                                          (cdr (assoc 'message (exn:fail:sql-info e))))
                           (let ([r (findf (disjoin relation? record?) formals)])
                             (create (or r (raise e)))
                             (apply f formals))
                           (raise e)))])
      (unless (current-db-connection)
        (current-db-connection (connect))
        (query-exec+ "pragma foreign_keys = on;"))
      (apply f formals))))


(define-db-procedure (create has-relation)
  (define relation-inst (gen-relation has-relation))
  (query-exec+
   (format "CREATE TABLE IF NOT EXISTS ~a (\n~a\n);"
           (relation-name relation-inst)
           (string-join (map (λ (i) (format "    ~a" i))
                             (relation-fields relation-inst))
                        ",\n"))))



(define (exists?/unsafe subquery . args)
  (eq? 1 (apply query-value+ (format "select exists from (select 1 from ~a);" subquery)
                args)))


(define-db-procedure (in-relation/unsafe relation-inst what)
  (in-query+ (~a "select " what " from " (relation-name relation-inst) ";")))


(define-db-procedure (load-by-id relation-inst record-ctor id)
  (apply record-ctor (vector->list
                      (query-row+ (~a "select * from " (relation-name relation-inst)
                                      " where id=?;")
                                  id))))


(define-db-procedure (load-by-record record-inst)
  (load-by-id (gen-relation record-inst)
              (gen-constructor record-inst)
              (record-id record-inst)))


(define-db-procedure (save record-inst)
  (define vals (vector-drop (struct->vector record-inst) 1))
  (define insert? (not (vector-ref vals 0)))
  (define id (or (vector-ref vals 0) sql-null))

  (apply query-exec+
         (~a (if insert? "insert" "replace")
             " into " (relation-name (gen-relation record-inst))
             " values ("
             (string-join (build-list (vector-length vals) (const "?")) ",")
             ");")
         (cons id (cdr (vector->list vals))))

  (if insert?
      (query-value+ "SELECT last_insert_rowid();")
      id))


(define-db-procedure (delete record-inst)
  (apply query-exec+
         (~a "delete from " (relation-name (gen-relation record-inst)) " where id=?;")
         (record-id record-inst)))


(define-db-procedure (path-declared? path)
  (exists?/unsafe (~a (relation-name paths) " where path=?")
                  path))


;------------------------------------------------------------------
; search-by-record constructs a SELECT query based on "holes" in a
; given record. In that sense, (search-by-record (customer #f "John"
; "Doe" #f #f)) returns a sequence of John Does in the database.
;
; The search goes by exact match, so construct your own SELECT
; if different comparisons are necessary.


(define-db-procedure (search-by-record record-inst [ctor (gen-constructor record-inst)])
  (define available-values (vector->list (vector-drop (struct->vector record-inst) 1)))
  (define query-args (infer-select-query record-inst available-values))
  (if (null? query-args)
      (in-value record-inst)
      (sequence-map (λ from-db (apply ctor (fill-holes null (reverse from-db) available-values)))
                    (apply in-query+ query-args))))


(define-db-procedure (find-exactly-one record-inst [ctor (gen-constructor record-inst)])
  (define seq (search-by-record record-inst ctor))
  (and (not (stream-empty? seq))
       (stream-empty? (stream-first (stream-rest seq)))
       (stream-first seq)))


(module+ test
  (test-case "Infer clauses for a SELECT query to find missing information"
    (call-with-values (λ () (infer-select-clauses '(192 #f "mark") '("id" "foo" "bar")))
                      (λ (cols conditions params)
                        (check-equal? cols '("foo"))
                        (check-equal? conditions '("bar=?" "id=?"))
                        (check-equal? params '("mark" 192))))))


(define (infer-select-clauses available-values cols)
  (for/fold ([wip-requested-columns null]
             [wip-conditions null]
             [wip-params null])
            ([val (in-list available-values)]
             [col (in-list cols)])
    (if val
        (values wip-requested-columns
                (cons (format "~a=?" col) wip-conditions)
                (cons val wip-params))
        (values (cons col wip-requested-columns)
                wip-conditions
                wip-params))))


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


(module+ test
  (test-case "Fill holes in available values from a record"
    (check-equal? (fill-holes null '(1 2 3) '(a #f 8 #f b #f))
                  '(a 1 8 2 b 3))
    (check-equal? (fill-holes null '(1 2 3) '(#f #f #f))
                  '(1 2 3)))
  (test-exn "Raise an error if there are more holes than data"
            exn:fail?
            (λ () (fill-holes null '(1 2 3) '(#f #f #f #f)))))




;----------------------------------------------------------------------------------
; Paths

(define (get-localstate-path)
  (build-workspace-path "var/xiden/db"))


(define (get-objects-directory)
  (build-workspace-path
   "var/xiden/objects"))


(define (build-object-path digest)
  (build-path (get-objects-directory)
              (encoded-file-name digest)))



;----------------------------------------------------------------------------------
; Transaction mechanism: Use DBMS transaction feature, such that a rollback
; leaves a discrepency between paths in the DB and existing paths on the filesystem.
; To roll back the filesystem, delete every file not declared in the database.

(define current-halt-transaction (make-parameter #f))

(define (halt-transaction)
  ((current-halt-transaction)))


(define-syntax-rule (transact body ...)
  (call-with-fs-transaction (λ () body ...)))


(define-db-procedure (rollback-fs-transaction)
  (with-handlers ([exn:fail:sql? void]) (query-exec+ "rollback transaction;"))
  (for ([path (in-directory (get-objects-directory))])
    (unless (path-declared? path)
      (delete-directory/files #:must-exist? #f path))))


(define-db-procedure (call-with-fs-transaction act!)
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


;----------------------------------------------------------------------------------
; These procedures control file output, such that each written file comes
; with a declaration in the database that the path exists and is valid.
;
; For a given path P:
;
;  - If DB declares P
;    * P exists: DB integrity is fine.
;    * P does not exist: DB integrity is not fine. Not recoverable.
;  - If DB does not declare P
;    * P exists: Filesystem integrity is fine.
;    * P does not exist: Should delete P.
;


(define (make-addressable-file name in est-size)
  (define tmp (build-object-path #"tmp"))
  (dynamic-wind
    void
    (λ ()
      (with-handlers ([values (λ (e) (delete-file* tmp) (raise e))])
        (make-directory* (path-only tmp))
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
        (declare-path path digest)
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


(define-db-procedure (declare-path path digest)
  (gen-save (path-record #f
                         (path->string
                          (if (complete-path? path)
                              (find-relative-path (workspace-directory) path)
                              path))
                         digest)))


(define-db-procedure (declare-derivation provider-name
                                         package-name
                                         edition-name
                                         revision-number
                                         revision-names
                                         output-path)
  (define provider-id  (save (provider-record provider-name)))
  (define package-id   (save (package-record package-name provider-id)))
  (define edition-id   (save (edition-record edition-name package-id)))
  (define revision-id  (save (revision-record revision-number edition-id)))

  (define revision-name-ids
    (for/list ([name (in-list revision-names)])
      (save (revision-name-record name revision-id))))

  (save (derivation-record output-path revision-id)))


(define-db-procedure (declare-link target link-path)
  (match-define (vector target-id digest)
    (query-row+ "select id, digest from "
                (relation-name paths)
                " where path=?;" target))

  (declare-path link-path digest)
  (define link-id (query-value+ "select id from " (relation-name paths) " where path=?;" link-path))
  (gen-save (link-record link-id target-id)))


(define-db-procedure (in-valid-derivations query)
  (match-define
    (xiden-query
     provider-name
     package-name
     edition-name
     revision-min-exclusive?
     revision-min
     revision-max-exclusive?
     revision-max)
    (coerce-xiden-query query))

  (call/cc
   (λ (return)
     ; A failure to match some queries
     ; implies that no derivations will match
     (define (q arg)
       (define rec-or-#f (find-exactly-one arg))
       (if rec-or-#f
           (record-id rec-or-#f)
           (return empty-stream)))

     (define provider-id (q (provider-record #f provider-name)))
     (define package-id  (q (package-record  #f package-name provider-id)))
     (define edition-id  (q (edition-record  #f edition-name package-id)))

     (define (revision->revision-number v)
       (cond [(revision-number? v) v]
             [(revision-number-string? v) (string->number v)]
             [else
              (q (revision-record #f v edition-id))]))

     (define-values (lo hi)
       (get-inclusive-revision-range #:named-interval "db"
                                     revision-min-exclusive?
                                     revision-max-exclusive?
                                     (revision->revision-number revision-min)
                                     (revision->revision-number revision-max)))

     (define revision-id
       (query-value+
        "select id from " (gen-relation revisions)
        " where edition_id=? and revision_number >= ? and revision_number <= ?;"
        edition-id
        lo
        hi))

     (search-by-record (derivation-record #f revision-id)))))
