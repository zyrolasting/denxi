#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [make-digest-file-path
   (-> path-string? symbol? path?)]
  [make-signature-file-path
   (-> path-string? path?)]
  [make-filesystem-shovel
   (->* (complete-path? symbol? source-variant?)
        shovel/c)]))


(require racket/format
         racket/match
         racket/path
         "../artifact.rkt"
         "../crypto.rkt"
         "../dig.rkt"
         "../input.rkt"
         "../integrity.rkt"
         "../subprogram.rkt"
         "../monad.rkt"
         "../signature.rkt"
         "../source.rkt")

(define (make-digest-file-path path chf)
  (string->path (~a path "." chf)))

(define (make-signature-file-path path)
  (string->path (~a path ".sig")))

(define ((make-filesystem-shovel directory-path chf public-key-source) key)
  (if (and (path-string? key)
           (not (complete-path? key)))
      (let ([complete-path (build-path directory-path key)])
        (if (file-exists? complete-path)
            (subprogram-unit
             (artifact (file-source (normalize-path complete-path))
                       (try-integrity complete-path chf)
                       (try-signature complete-path chf public-key-source)))
            (dig-failure 'filesystem-shovel key)))
      (dig-failure 'filesystem-shovel key)))


(define (try-integrity complete-path chf)
  (let ([s (try-file-source (make-digest-file-path complete-path chf))])
    (and s (integrity chf s))))


(define (try-signature complete-path chf public-key-source)
  (let ([s (try-file-source (make-signature-file-path (make-digest-file-path complete-path chf)))])
    (and s (signature public-key-source s))))


; Source availability is based on file availability.
(define (try-file-source path)
  (and (file-exists? path)
       (file-source (normalize-path path))))


; Lower-level path building procedures
(define (try-revision-number-from-path . els)
  (let ([path (apply build-path els)])
    (and (file-exists? path)
         ; normalize-path resolves symlinks
         (string->number (~a (file-name-from-path (normalize-path path)))))))


(module+ test
  (require racket/format
           racket/file
           racket/function
           racket/port
           racket/runtime-path
           rackunit
           "../file.rkt"
           "../string.rkt")

  (define (touch . paths)
    (for ([path (in-list paths)])
      (display-to-file "" path)))

  (define chf 'md5)
  (define public-key-source (byte-source #""))

  (define (make-artifact-paths path)
    (define digest-path (~a path "." chf))
    (define sig-path (~a digest-path ".sig"))
    (touch path)
    (touch digest-path)
    (touch sig-path)
    (values path
            digest-path
            sig-path))

  (define (check-source path src)
    (check-pred file-source? src)
    (check-equal? (file-or-directory-identity path)
                  (file-or-directory-identity (file-source-path src))))

  
  (test-case "Parse revision number from paths"
    (call-with-temporary-directory
     #:cd? #t
     (λ (directory-path)
       (touch "0")
       (make-file-or-directory-link "0" "L")
       (define actual (try-revision-number-from-path directory-path "0"))
       (check-equal? actual 0)
       (test-case "Parse revision number from links and alternative paths"
         (check-equal? (try-revision-number-from-path directory-path "L") actual)
         (check-equal? (try-revision-number-from-path "0") actual)
         (check-equal? (try-revision-number-from-path "L") actual))
       (test-false "Fail parsing revision numbers from paths using #f"
                   (try-revision-number-from-path directory-path "junk")))))


  (test-case "Bind filesystem digs"
    (call-with-temporary-directory
     #:cd? #t
     (λ (directory-path)
       (define dig (make-filesystem-shovel directory-path chf public-key-source))

       (make-directory* "a/b")
       (define-values (content-path digest-path sig-path)
         (make-artifact-paths "a/b/f"))

       (test-case "Find full artifact in filesystem dig"
         (define result (get-subprogram-value (dig content-path)))
         (check-pred artifact? result)

         (match-define
           (artifact c
                     (integrity actual-chf i)
                     (signature actual-pk s))
           result)
         (check-eq? public-key-source actual-pk)
         (check-eq? actual-chf chf)
         (check-source content-path c)
         (check-source digest-path i)
         (check-source sig-path s))

       (test-case "Find partial artifact in filesystem dig"
         (delete-file sig-path)
         (delete-file digest-path)
         (define partial (get-subprogram-value (dig content-path)))
         (check-source content-path (artifact-source partial))
         (check-false (artifact-integrity partial))
         (check-false (artifact-signature partial)))

       (test-case "Resolve symlinks in filesystem dig"
         (make-file-or-directory-link content-path "link")
         (define linked (get-subprogram-value (dig "link")))
         (check-source content-path (artifact-source linked))
         (check-false (artifact-integrity linked))
         (check-false (artifact-signature linked)))))))
