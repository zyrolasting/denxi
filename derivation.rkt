#lang racket/base

; Define a way to create arbitrary and unique directories in terms of
; inputs.
;
; Requirements:
;
; - Verify the content of a directory.
; - Recover from catestrophic failure (e.g. power outage)

(provide (struct-out zcpkg-derivation)
         build-derivation!
         in-build-schedule
         in-derivations)

(require racket/exn
         racket/format
         racket/function
         racket/generator
         racket/match
         racket/path
         racket/port
         racket/sequence
         net/base64
         "db.rkt"
         "file.rkt"
         "integrity.rkt"
         "verify.rkt")


(struct zcpkg-derivation (name inputs process!)
  #:property prop:procedure
  (λ (self dir)
    (parameterize ([current-directory dir])
      (for ([output (in-directory)])
        ((zcpkg-derivation-process! self))))))


(define (in-broken-derivations)
  (sequence-filter (λ (row)
                     (match-define (vector id name digest errors) row)
                     (not (sql-null? errors)))
                   (in-derivations)))


(define (in-build-schedule drvs)
  (define (schedule-build drv)
    (sequence-for-each schedule-build (in-list (zcpkg-derivation-inputs drv)))
    (yield drv))
  (in-generator
   (for ([drv (in-list drvs)])
     (schedule-build drv))))


(define (in-derivations)
  (in-query+ "select * from derivations;"))


(define (make-derivation-directory-name digest name)
  (format "~a-~a" digest name))


(define (delete-broken-derivations basedir)
  (for ([broken (in-broken-derivations)])
    (match-define (vector _ name digest _) broken)
    (with-handlers ([exn:fail:filesystem? void])
      (delete-directory/files
       (build-path basedir (make-derivation-directory-name digest name))))))


(define (build-derivation! basedir drv)
  (define name (zcpkg-derivation-name drv))
  (define digest (make-derivation-hash drv))
  (define hash-string (base64-encode digest #""))
  (define dir (build-path basedir (make-derivation-directory-name digest name)))

  (if (valid-derivation-exists? digest dir)
      dir
      (let ([errors
             (with-handlers ([(const #t) (λ (e) (list (exn->string e)))])
               (make-directory dir)
               (drv dir))])
        (record-derivation-state! drv errors)
        (and (null? errors)
             dir))))

(define (valid-derivation-exists? digest dir)
  (and (directory-exists? dir)
       (sql-null? (query-maybe-value+ "select errors from derivations where digest=?;"
                                      digest))))

(define (make-derivation-hash drv)
  (make-digest
   (apply input-port-append
          (map open-input-bytes
               (sequence->list
                (in-generator
                 (yield (string->bytes/utf-8 (zcpkg-derivation-name drv)))
                 (for ([input-drv (in-list (zcpkg-derivation-inputs drv))])
                   (yield (make-derivation-hash drv)))))))))


(define (record-derivation-state! drv digest errors)
  (define error-datum (if (null? errors) sql-null (~s errors)))
  (query-exec+ "insert into derivations values (NULL, ?, ?, ?) on conflict(digest) do update set errors=?;"
               (zcpkg-derivation-name drv)
               digest
               error-datum
               error-datum))


(define (make-items-table)
(query-exec+ #<<EOS
CREATE TABLE IF NOT EXISTS items (
       id INTEGER NOT NULL PRIMARY KEY,
       derivation_id INTEGER NOT NULL,
       path TEXT,
       digest BLOB,
       FOREIGN KEY derivation_id REFERENCES derivations(id)
);
EOS
))


(define (make-derivation-table)
(query-exec+ #<<EOS
CREATE TABLE IF NOT EXISTS derivations (
       id INTEGER NOT NULL PRIMARY KEY,
       name TEXT,
       digest BLOB,
       errors TEXT
);
EOS
))
