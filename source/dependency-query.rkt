#lang racket/base

(provide dependency-string?
         parse-dependency-string
         make-dependency-query
         zcpkg-info->dependency-query)

(require racket/match
         "base.rkt"
         "revision.rkt"
         "../zcpkg-info.rkt")

(struct dependency-query (publisher brand artifact edition revision)
  #:transparent)

(define (make-pattern-string fmt-str)
  (format fmt-str
          name-pattern-string
          name-pattern-string
          name-pattern-string
          name-pattern-string
          revision-pattern-string))

(define dependency-string?
  (make-rx-predicate (make-pattern-string "~a/~a(?:/~a(?:/~a(?:/~a)?)?)?")))

(define capture-in-dependency-string
  (make-rx-matcher (make-pattern-string "(~a)/(~a)(?:/(~a)(?:/(~a)(?:/(~a))?)?)?")))

(define (parse-dependency-string v)
  (match (capture-in-dependency-string v)
    [(list _ args ...)
     (apply make-dependency-query args)]
    [_ #f]))

(define (make-dependency-query publisher brand [artifact #f] [edition #f] [revision-query #f])
  (dependency-query publisher
                    brand
                    (or artifact "default")
                    (or edition "draft")
                    (or revision-query "newest")))

(define (zcpkg-info->dependency-query info)
  (dependency-query (zcpkg-info-publisher info)
                    (zcpkg-info-brand info)
                    (zcpkg-info-artifact info)
                    (zcpkg-info-edition info)
                    (zcpkg-info-revision-number info)))
