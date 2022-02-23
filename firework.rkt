#lang racket/base

(require racket/contract
         racket/file
         racket/function
         racket/format
         racket/generator
         racket/match
         racket/sequence)


(provide powder
         (contract-out
          [powder/c contract?]
          [firework
           (-> simple-file-name?
               powder/c
               list?)]))



(define powder/c
  (hash/c (and/c path-string?
                 (not/c complete-path?))
          (cons/c (integer-in 0 65535)
                  bytes?)))


(define (simple-file-name? path)
  (match (map ~a (explode-path path))
    [(list (not (or ".." "."))) #t]
    [_ #f]))


(define (firework directory-name to-pack)
  `(module packed racket/base
     (require racket/file
              racket/function
              racket/runtime-path
              racket/path)
     (define-runtime-path .r ".")
     (define ./ (curry build-path .r ,(~a directory-name) a))
     (define (& p r d . xs)
       (unless (complete-path? p)
         (define dp (path-only p))
         (when dp (make-directory* dp))
         (call-with-output-file p
           #:permissions r
           (curry write-bytes d))
         (unless (null? xs) (apply & xs))))
     (module+ main
       (make-directory (./))
       (current-directory (./))
       (& . ,(sequence->list
               (in-generator
                (for ([(path item) to-pack])
                  (yield (~a path))
                  (yield (car item))
                  (yield (cdr item)))))))))


(define-syntax-rule (powder (pm [r c] ...) ...)
  (make-immutable-hash
   (append (let ([p pm]) (list (cons r (cons p c)) ...))
           ...)))


(module+ main
  (require racket/cmdline
           racket/path)

  (define (make-powder path)
    (cons (file-or-directory-permissions path 'bits)
          (file->bytes path)))

  (define (expect-file path)
    (unless (file-exists? path)
      (raise-user-error 'firework "non-file: ~a" path))
    path)

  (define ((given-name path name) h)
    (hash-set h name (make-powder (expect-file path))))

  (define ((reuse-name path) h)
    (hash-set h
              (file-name-from-path path)
              (make-powder (expect-file path))))

  (define (parse [argv (current-command-line-arguments)])
    (parse-command-line "firework"
                        argv
                        `((multi
                           [("+g")
                            ,(λ (flag path name)
                               (given-name path name))
                            ("pack file, give name" "path" "name")]))
                        (λ (given dirname . reusable)
                          (define unpacked (append given (map reuse-name reusable)))
                          (firework dirname
                                    (for/fold ([packed (hash)])
                                              ([pack unpacked])
                                      (pack packed))))
                        '("dirname" "file")))

  (writeln (parse)))
