#lang racket/base

; A lock file is responsible for reproducing a directory.

(require racket/contract)

(require racket/serialize)

(define LOCK-FILE-NAME "lock.rktd")

; A lock on a package directory. An instance can reproduce a directory of remote resources.
(serializable-struct pkg-directory-lock (dirname linkpath pkg-locks))

; A lock on a particular package state
(serializable-struct pkg-lock (pkg ssi))

#|


(provide
 (struct-out pkg-directory-lock)
 (struct-out pkg-lock)
 (contract-out
  [LOCK-FILE-NAME path-string?]
  [pkg-lock/c contract?]
  [pkg-directory-lock/c contract?]
  [format-pkg-lock (-> pkg-lock/c string?)]
  [format-directory-lock (-> pkg-directory-lock/c string?)]))

(define pkg-lock/c (struct/c pkg-lock pkg/c ssi/c))
(define pkg-directory-lock/c (struct/c pkg-directory-lock path-string? path-string? (listof pkg-lock/c)))

(define (format-pkg-lock lock)
  (define pkg (pkg-lock-pkg lock))
  (format "~a [~a] [~a]"
          (package-name pkg)
          (package-state pkg)
          (package-lock-pkg-ssi lock)))

(define (format-directory-lock dirlock)
  (format "Lock: ~a~nLink: ~a~n~n~a~n"
          (pkg-directory-lock-dirname dirlock)
          (pkg-directory-lock-linkpath dirlock)
          (string-join (map (λ (entry) (string-append "  " (format-pkg-lock entry)))
                            (pkg-directory-lock-pkg-locks dirlock))
                       "/")))

(define (write-lock-file dirlock)
  (call-with-output-file LOCK-FILE-NAME #:exists 'truncate/replace
    (λ (o) (s-exp->fasl (serialize dirlock) o))))

(define (read-lock-file)
  (call-with-input-file LOCK-FILE-NAME
    (λ (in) (deserialize (fasl->s-exp in)))))
|#
