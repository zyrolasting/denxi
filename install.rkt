#lang racket/base

(provide install)

(require idiocket/file
         idiocket/path
         idiocket/function
         "archiving.rkt"
         "download.rkt"
         "path.rkt"
         "source/dependency-query.rkt"
         "url.rkt"
         "zcpkg-info.rkt")


; Realize that `install` is the only procedure called for its
; effect. All other procedures return thunks that represent the next
; step for `install`.

(define (install source)
  (apply-until (source->thunk source)))

; A package source is a string with many possible meanings. Map the
; string to an URL for context. Pick the next step depending on what
; the URL means.
(define (source->thunk v)
  (cond [(string? v) (source->thunk (string->url v))]
        [(find-catalog-name v) => (apply-later download-from-catalog)]
        [(find-directory-path v) => (apply-later path->installation-thunk)]
        [else (raise-argument-error 'resolve-source
                                    "A valid package source"
                                    v)]))

; Make a procedure P that can be used to delay application of another
; procedure in (cond [test-expr => P]).
;
; ex: (= 2 (cond [1 => (apply-later +)]))
;
(define (apply-later f)
  (λ a (λ () (apply f a))))

(define (path->installation-thunk source-path)
  (define info (read-zcpkg-info (build-path source-path "info.rkt")))
  (if (zcpkg-installed? info)
      (λ () (report-already-installed info))
      (λ () (install-in-current-directory source-path info))))

(define (install-in-current-directory source-path info)
  (copy-directory/files source-path
                        (build-path (current-directory)

(define (zcpkg-installed? info revision-min/inclusive revision-max/inclusive)
  (and (equal? name (zcpkg-info-name info))
       (equal? publisher (zcpkg-info-publisher info))
       (equal? edition (zcpkg-info-edition info))
       (>= (zcpkg-info-revision-number info) revision-min/inclusive)
       (<= (zcpkg-info-revision-number info) revision-max/inclusive)))


(define (download-from-catalog info-url)
  (define artifact-info (download-artifact-info info-url))
  (define artifact-path (download-artifact artifact-info))
  (define zcpackage-path (unpack artifact-path))
  (path->installation-thunk zcpackage-path))
