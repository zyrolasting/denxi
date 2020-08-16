#lang racket/base

; Define a way to create arbitrary and unique directories in terms of
; inputs.
;
; Requirements:
;
; - Verify the content of a directory.
; - Recover from catestrophic failure (e.g. power outage)

(provide (struct-out zcpkg-derivation-output)
         (struct-out zcpkg-derivation)
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
         "store.rkt"
         "verify.rkt")

(struct zcpkg-derivation-output (expected-digest path make!))

(struct zcpkg-derivation (name inputs outputs)
  #:property prop:procedure
  (λ (self dir)
    (parameterize ([current-directory dir])
      (for ([output (in-list (zcpkg-derivation-outputs self))])
        (match-define (zcpkg-derivation-output expected-digest path make!) output)
        (make! (zcpkg-derivation-inputs self))
        (if expected-digest
            (unless (equal? (make-digest path) expected-digest)
              (error (format "~s failed to produce the exact ~s it promised."
                             (zcpkg-derivation-name self)
                             path)))
            ; Use this to trigger an error if the output path does not exist.
            (void (file-or-directory-identity path)))))))


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

(define (delete-broken-derivations store)
  (for* ([broken (in-broken-derivations)]
         [path (zcpkg-derivation-output broken)])
    (with-handlers ([exn:fail:filesystem? void])
      (delete-directory/files path))))


(define (build-derivation! basedir drv)
  (define name (zcpkg-derivation-name drv))
  (define digest (make-derivation-hash drv))
  (define hash-string (base64-encode digest #""))
  (define dir (build-path basedir (format "~a-~a" digest name)))

  (define errors
    (with-handlers ([(const #t) (λ (e) (list (exn->string e)))])
      (make-directory dir)
      (drv dir)))

  (record-derivation-state! drv errors)
  (and (null? errors)
       dir))


(define (make-derivation-hash drv)
  (make-digest
   (apply input-port-append
          (map open-input-bytes
               (sequence->list
                (in-generator
                 (yield (string->bytes/utf-8 (zcpkg-derivation-name drv)))
                 (for ([input-drv (in-list (zcpkg-derivation-inputs drv))])
                   (yield (make-derivation-hash drv)))
                 (for ([output (in-list (zcpkg-derivation-outputs drv))])
                   (match-define (zcpkg-derivation-output expected-digest path make!) output)
                   (when expected-digest
                     (yield expected-digest)))))))))


(define (record-derivation-state! drv digest errors)
  (define error-datum (if (null? errors) sql-null (~s errors)))
  (query-exec+ "insert into derivations values (NULL, ?, ?, ?) on conflict(digest) do update set errors=?;"
               (zcpkg-derivation-name drv)
               digest
               error-datum
               error-datum))


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
