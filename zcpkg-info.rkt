#lang racket/base

; Responsible for helping others understand the purpose and release
; information of a package.

(provide (struct-out zcpkg-info)
         read-zcpkg-info
         write-zcpkg-info
         getinfo/zcpkg)

(require idiocket/file
         "metadata.rkt")

(struct zcpkg-info
  (distributor     ; The name of the party distributing the artifact in a catalog.
   project         ; The name of the project submitted by a distributor.
   artifact        ; The name of an artifact in a project.
   edition         ; The name of an artifact's design.
   revision-number ; The number of a design's implementation.
   revision-names  ; Aliases for the revision-number.
   installer       ; A Racket module responsible for userspace changes.
   dependencies)   ; A list of dependency queries.
  #:transparent)

(declare-info-i/o zcpkg-info)

(define (zcpkg-get-info-file-path p)
  (or (get-path-if-file-exists (build-path p "info.rkt"))
      (get-path-if-file-exists (build-path p "zcp-info.rkt"))))

(define (zcpkg-get-info p)
  (read-zcpkg-info (zcpkg-get-info-file-path p)))

(define (zcpkg-comparable? a b)
  (for/and ([p (in-list (list zcpkg-info-distributor
                              zcpkg-info-edition
                              zcpkg-info-artifact))])
    (equal? (p a) (p b))))

(define (zcpkg-compare-versions a b)
  (and (zcpkg-comparable? a b)
       (- (zcpkg-info-revision-number a)
          (zcpkg-info-revision-number b))))
