#lang racket/base

; Define metadata as persistent values.
; Use the filesystem until a DB is warranted.

(require racket/file
         racket/match
         racket/path
         "contract.rkt")


(provide
 (contract-out
  [set-metadatum!
   (->* (flat-contract? any/c path?) (#:preamble (listof string?)) void?)]
  [get-metadatum
   (->* (flat-contract? path?) (#:optional? any/c) any/c)]
  [get-metadata
   (-> path? (listof (list/c any/c flat-contract? path-string?)) list?)]
  [set-metadata!
   (-> path? (listof (list/c flat-contract? any/c path-string?)) void?)]))


(define (get-metadatum #:optional? [optional? #f] cnt path)
  (define lockfile (make-lock-file-name path))
  (dynamic-wind
    void
    (λ ()
      (with-handlers ([exn?
                       (λ (e)
                         (cond [(exn:fail:contract? e)
                                (raise (rewrite-contract-error-message e (file-name-from-path path)))]
                               [(exn:fail:filesystem? e)
                                (if optional? #f (raise e))]
                               [else (raise e)]))])
        (call-with-file-lock/timeout
         path 'shared
         (λ () (invariant-assertion cnt (call-with-input-file path read)))
         (λ () (error 'get-metadatum!
                      "Failed to obtain lock for ~a"
                      path)))))
    (λ ()
      (when (file-exists? lockfile)
        (delete-file lockfile)))))


(define (set-metadatum! #:preamble [preamble null] cnt value path)
  (invariant-assertion cnt value)
  (define lockfile (make-lock-file-name path))
  (call-with-file-lock/timeout
   path 'exclusive
   (λ ()
     (call-with-output-file #:exists 'truncate/replace
       path (λ (o)
              (for ([line (in-list preamble)])
                (display "; " o)
                (displayln line o))
              (writeln value o))))
   (λ () (error 'set-metadatum!
                "Failed to obtain lock for ~a"
                path)))
  (delete-file lockfile))


(define (get-metadata dir requirements)
  (for/list ([requirement (in-list requirements)])
    (match-define (list optional? cnt filename) requirement)
    (get-metadatum #:optional? optional? cnt (build-path dir filename))))


(define (set-metadata! dir requirements)
  (for ([requirement (in-list requirements)])
    (match-define (list cnt val filename) requirement)
    (set-metadatum! cnt val (build-path dir filename))))

(module+ test
  (require racket/file
           racket/port
           rackunit)

  (define tmp-file (make-temporary-file))

  (define (save-real v)
    (set-metadatum! #:preamble (list "a comment" "another comment")
                    real? v tmp-file))

  (dynamic-wind
    void
    (λ ()
      (save-real 10)
      (test-eq? "Exchange metadatum with disk"
                (get-metadatum real? tmp-file)
                10)

      (test-exn "Signal contract violations on read"
                exn:fail:contract?
                (λ () (get-metadatum string? tmp-file)))

      (test-case "Protect file from invalid values"
        (check-exn exn:fail:contract?
                   (λ () (save-real "not real")))
        (check-eq? (get-metadatum real? tmp-file)
                   10))

      (test-false "Optional metadatum is #f if file does not exist"
                  (get-metadatum #:optional? #t real? "/var/nonselahdjdkfs"))

      (test-exn "Optional metadatum must obey contract if file does exist"
                exn:fail:contract?
                (λ ()
                  (get-metadatum #:optional? #t string? tmp-file))))

    (λ () (delete-file tmp-file))))
