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
           (-> void?)]
          [declare-output
           (-> non-empty-string?
               non-empty-string?
               non-empty-string?
               exact-nonnegative-integer?
               (listof non-empty-string?)
               string?
               path-record?
               output-record?)]
          [call-with-reused-output
           (-> xiden-query-variant?
               (-> (or/c #f exn? output-record?) any)
               any)]
          [in-xiden-outputs
           (-> xiden-query-variant?
               (sequence/c output-record?))]
          [in-path-links
           (-> path-record? (sequence/c path-record?))]
          [find-exactly-one
           (->* (record?) (procedure?) (or/c #f record?))]
          [start-fs-transaction
           (-> (values (-> void?) (-> void?)))]
          [build-object-path
           (->* () #:rest (listof path-string?) complete-path?)]
          [build-addressable-path
           (-> bytes? complete-path?)]
          [in-xiden-objects
           (-> xiden-query-variant?
               (sequence/c path-string?
                           exact-positive-integer?
                           revision-number?
                           exact-positive-integer?
                           path-string?))]
          [make-addressable-file
           (-> non-empty-string? input-port? (or/c +inf.0 exact-positive-integer?) path-record?)]
          [make-addressable-directory
           (-> string? (-> complete-path? any) path-record?)]
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

(define-relation paths (path digest target-id)
  "path TEXT UNIQUE NOT NULL"
  "digest BLOB"
  "target_id INTEGER"
  "FOREIGN KEY (target_id) REFERENCES paths(id) ON DELETE RESTRICT ON UPDATE CASCADE")

(define-relation providers (provider-name)
  "name TEXT NOT NULL")

(define-relation packages (name provider-id)
  "name TEXT NOT NULL"
  "provider_id INTEGER NOT NULL"
  "FOREIGN KEY (provider_id) REFERENCES providers(id)")

(define-relation editions (name package-id)
  "name TEXT NOT NULL"
  "package_id INTEGER NOT NULL"
  "FOREIGN KEY (package_id) REFERENCES packages(id)")

(define-relation revisions (number edition-id)
  "number INTEGER NOT NULL"
  "edition_id INTEGER NOT NULL"
  "FOREIGN KEY (edition_id) REFERENCES editions(id)")

(define-relation revision-names (revision-name revision-id)
  "name TEXT NOT NULL"
  "revision_id INTEGER NOT NULL"
  "FOREIGN KEY (revision_id) REFERENCES revisions(id)")

(define-relation outputs (revision-id path-id name)
  "revision_id INTEGER NOT NULL"
  "path_id INTEGER NOT NULL"
  "name TEXT NOT NULL"
  "FOREIGN KEY (revision_id) REFERENCES revisions(id) ON DELETE CASCADE ON UPDATE CASCADE"
  "FOREIGN KEY (path_id)     REFERENCES paths(id) ON DELETE CASCADE ON UPDATE CASCADE")



;----------------------------------------------------------------------------------
; DB procedures are normal procedures that lazily prepare a connection
; and/or create missing tables.

(define-syntax-rule (define-db-procedure (id sig ...) body ...)
  (define id (make-db-procedure (λ (sig ...)  body ...))))


(define (connect)
  (define db-path ((current-get-localstate-path)))
  (make-directory* (path-only db-path))
  (sqlite3-connect #:database db-path
                   #:mode 'create
                   #:use-place #f))



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


(define (xiden-collect-garbage)
  (define (in-unreferenced-paths)
    (define referenced-paths
      (~a "select target_id from "
          (relation-name paths)
          " where target_id is not NULL"))
    (in-query+
     (~a "select P.id,P.path from " (relation-name paths) " as P "
         " where target_id is NULL and "
         " id not in (" referenced-paths ");")))

  (define (remove-dead-links)
    (define links (~a "select id,path from " (relation-name paths) " where target_id is not NULL"))
    (for ([(id path) (in-query+ links)])
      (unless (link-exists? path)
        (delete-record (path-record id #f #f #f #f)))))

  (create paths)
  (parameterize ([current-directory (workspace-directory)])
    (remove-dead-links)
    (for ([(id path) (in-unreferenced-paths)])
      (delete-directory/files path #:must-exist? #f)
      (delete-record (path-record id #f #f #f #f)))))


(define (create has-relation)
  (define relation-inst (gen-relation has-relation))
  (query-exec+
   (format "CREATE TABLE IF NOT EXISTS ~a (\n~a\n);"
           (relation-name relation-inst)
           (string-join (map (λ (i) (format "    ~a" i))
                             (relation-fields relation-inst))
                        ",\n"))))


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
  (define existing-or-#f (find-exactly-one record-inst))
  (define id (or (vector-ref vals 0)
                 (if existing-or-#f
                     (record-id existing-or-#f)
                     sql-null)))
  (define insert? (sql-null? id))

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


(define-db-procedure (delete-record record-inst)
  (apply query-exec+
         (~a "delete from " (relation-name (gen-relation record-inst)) " where id=?;")
         (record-id record-inst)))



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


; Take the name literally! This returns #f if a query returns more than one record.
(define-db-procedure (find-exactly-one record-inst [ctor (gen-constructor record-inst)])
  (define seq (search-by-record record-inst ctor))
  (and (with-handlers ([values (const #t)]) (sequence-ref seq 1) #f)
       (with-handlers ([values (const #f)]) (sequence-ref seq 0))))


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




;----------------------------------------------------------------------------------
; Paths

(define current-get-localstate-path ; Is a parameter for testing reasons.
  (make-parameter (λ () (build-workspace-path "var/xiden/db"))))


(define build-object-path
  (make-workspace-path-builder "var/xiden/objects"))

(define (build-addressable-path digest)
  (build-object-path (encoded-file-name digest)))


;----------------------------------------------------------------------------------
; Transaction mechanism: Use DBMS transaction feature, such that a rollback
; leaves a discrepency between paths in the DB and existing paths on the filesystem.
; To roll back the filesystem, delete every file not declared in the database.


(define-db-procedure (rollback-fs-transaction)
  (with-handlers ([exn:fail:sql? void]) (query-exec+ "rollback transaction;"))
  (for ([path (in-directory (build-object-path))])
    (unless (find-exactly-one (path-record #f (normalize-path-for-db path) #f))
      (delete-directory/files #:must-exist? #f path))))


(define-db-procedure (start-fs-transaction)
  (with-handlers ([values void]) (query-exec+ "begin exclusive transaction;"))
  (values (λ () (query-exec+ "commit transaction;"))
          (λ () (rollback-fs-transaction))))

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

(define-db-procedure (make-addressable-file name in est-size)
  (define tmp (build-addressable-path #"tmp"))
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
        (define path (build-addressable-path digest))
        (make-directory* (path-only path))
        (rename-file-or-directory tmp path #t)
        (declare-path (find-relative-path (workspace-directory) path) digest))
    (λ () (close-input-port in))))


(define-db-procedure (make-addressable-directory output-name proc)
  (call-with-temporary-directory
   #:cd? #t #:base (build-object-path)
   (λ (path)
     (proc path)
     (define digest (make-directory-content-digest output-name path))
     (define dest
       (build-path (path-only path)
                   (encoded-file-name digest)))
     (with-handlers ([exn:fail:filesystem?
                      (λ (e)
                        (copy-directory/files path dest #:preserve-links? #t)
                        (delete-directory/files path))])
       (rename-file-or-directory path dest #t))
     (declare-path (find-relative-path (workspace-directory) dest)
                   digest))))

(define-db-procedure (make-addressable-link target-path-record link-path)
  (make-file-or-directory-link
   (find-relative-path (path-only (simplify-path (path->complete-path link-path)))
                       (build-workspace-path (path-record-path target-path-record)))
   link-path)
  (gen-save (path-record #f
                         link-path
                         sql-null
                         (record-id target-path-record))))


(define (make-directory-content-digest output-name path)
  (for/fold ([dig (make-digest (open-input-bytes (string->bytes/utf-8 output-name)) 'sha384)])
            ([subpath (in-directory path)]
             #:when (file-exists? subpath))
    (call-with-input-file subpath
      (λ (in) (make-digest (input-port-append #t (open-input-bytes dig) in)
                           'sha384)))))


(define (print-transfer-status m)
  (write-message m
   (message-formatter
    [($transfer-progress name bytes-read max-size timestamp)
     (format "~a: ~a%" name (~r (* 100 (/ bytes-read max-size)) #:precision 0))]
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


(define-db-procedure (declare-path unnormalized-path digest)
  (gen-save (path-record #f (normalize-path-for-db unnormalized-path) digest sql-null)))


(define-db-procedure (declare-link unnormalized-path path-id)
  (gen-save (path-record #f
                         (normalize-path-for-db unnormalized-path)
                         sql-null
                         path-id)))


(define-db-procedure (in-path-links path-id)
  (search-by-record (path-record #f #f #f path-id)))


(define (normalize-path-for-db path)
  (if (string? path)
      path
      (path->string path)))


(define-db-procedure (declare-output provider-name
                                     package-name
                                     edition-name
                                     revision-number
                                     revision-names
                                     output-name
                                     output-path-record)
  (define provider-id  (save (provider-record #f provider-name)))
  (define package-id   (save (package-record  #f package-name provider-id)))
  (define edition-id   (save (edition-record  #f edition-name package-id)))
  (define revision-id  (save (revision-record #f revision-number edition-id)))
  (define revision-name-ids
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



(define-db-procedure (in-xiden-objects query-variant)
  (define query (coerce-xiden-query query-variant))
  (match-define
    (xiden-query
     provider-name
     package-name
     edition-name
     revision-min
     revision-max
     interval-bounds
     output-name)
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

     (define (revision->revision-number v)
       (or (find-revision-number v edition-id)
           (fail)))

     (define-values (lo hi)
       (with-handlers ([exn:fail? fail])
         (get-xiden-query-revision-range #:named-interval "db"
                                         #:lo (revision->revision-number revision-min)
                                         #:hi (revision->revision-number revision-max)
                                         query)))

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


(define-db-procedure (in-xiden-outputs query-variant)
  (sequence-map (λ (rid rn on path) (find-exactly-one (output-record #f rid #f #f)))
                (in-xiden-objects query-variant)))


(define-db-procedure (output-record->query output-record-inst)
  (match-define (vector provider-name package-name edition-name revision-number)
    (query-row+ (~a "select P.name, K.name, E.name, R.number"
                    " from "       (relation-name providers) " as P"
                    " inner join " (relation-name packages)  " as K on P.id = K.provider_id"
                    " inner join " (relation-name editions)  " as E on K.id = E.package_id"
                    " inner join " (relation-name revisions) " as R on E.id = R.edition_id"
                    " where R.id = ?")
                (output-record-revision-id output-record-inst)))
  (xiden-query provider-name
               package-name
               edition-name
               (~a revision-number)
               (~a revision-number)
               "ii"
               (output-record-name output-record-inst)))


(define-db-procedure (call-with-reused-output query continue)
  (continue
   (call/cc
    (λ (k)
      (define-values (output-name rev-id rev-no path-id path)
        (with-handlers ([values k])
          (sequence-ref (in-xiden-objects query) 0)))
      (find-exactly-one (output-record #f rev-id #f output-name))))))


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

  (test-db "Declare a path"
    (declare-path "a/b/c" #"digest")
    (check-equal? (find-exactly-one (path-record #f "a/b/c" #f #f))
                  (path-record 1 "a/b/c" #"digest" sql-null))
    (test-exn "Forbid duplicate paths"
              exn:fail?
              (λ ()
                (declare-path "a/b/c" #"different"))))


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
      (declare-output (xiden-query-provider-name query)
                      (xiden-query-package-name query)
                      (xiden-query-edition-name query)
                      revision-number
                      revision-names
                      (~a "out" revision-number)
                      (declare-path (~a "path" revision-number)
                                    #"abcd")))

    (define (mock-install-line qs N)
      (define query (coerce-xiden-query qs))
      (for ([i (in-range N)])
        (mock-install query i (list (format "rev-~a" i)
                                    (format "alt-~a" i)))))

    (mock-install-line "a.example.com:widget:default" 10)

    (define actual-results
      (sequence->list
       (in-values-sequence
        (in-xiden-objects "a.example.com:widget:default:rev-3:7:ei:lib"))))

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
