#lang racket/base

; Extract archives with minimal user involvement.

(require racket/contract)

(provide
 (contract-out
  [extract (-> (or/c path-string? input-port?) (logged/c void?))]))

(require racket/format
         racket/match
         racket/path
         racket/string
         file/tar
         file/untar
         file/untgz
         file/unzip
         "file.rkt"
         "logged.rkt"
         "message.rkt"
         "plugin.rkt")

(define+provide-message $extract-report (status target))


(define (extract variant)
  (logged
   (λ (messages)
     (let start ([in variant])
       (if (path-string? in)
           (call-with-input-file in start)
           (let* ([path (object-name in)]
                  [proc (get-extract-procedure path)])
             (if proc
                 (values (void (proc in))
                         (cons ($extract-report 'done path)
                               messages))
                 (values FAILURE
                         (cons ($extract-report 'unsupported path)
                               messages)))))))))


(define (get-extract-procedure name)
  (match (reverse (string-split (~a (file-name-from-path name)) "."))
    [(list "tar" _ ...)
     untar]
    [(or (list "tgz" _ ...) (list "gz" "tar" _ ...))
     untgz]
    [(list "zip" _ ...)
     unzip]
    [else
     (define ext (let ([fail (λ _ #f)]) (load-from-plugin 'get-extract-procedure fail fail)))
     (invariant-assertion (or/c #f (-> input-port? void?))
                          (and (procedure? ext)
                               (ext name)))]))


(module+ test
  (require rackunit "rc.rkt")

  (define (make-file path-string)
    (define path (string->path path-string))
    (make-directory* (path-only path))
    (display-to-file "data" path)
    path)

  (define (test-file path)
    (test-equal? (format "~a restored" path)
                 (file->string path)
                 "data"))

  (test-case "Select correct algorithms based on file extension"
    (check-equal? (get-extract-procedure "a..tar") untar)
    (check-equal? (get-extract-procedure "a.tar") untar)
    (check-equal? (get-extract-procedure "a.tgz") untgz)
    (check-equal? (get-extract-procedure "a.zip") unzip)
    (check-equal? (get-extract-procedure "a.extra....tar.gz") untgz)
    (check-equal? (get-extract-procedure "a.blah") #f)

    (define plugin-path (make-temporary-file))
    (dynamic-wind void
                  (λ ()
                    (call-with-output-file #:exists 'truncate/replace plugin-path
                      (λ (o)
                        (writeln
                         '(module anonymous racket/base
                            (provide get-extract-procedure)
                            (define (get-extract-procedure name)
                              (λ (in) (void (read-byte in)))))
                         o)))

                    (XIDEN_PLUGIN_MODULE
                     plugin-path
                     (λ ()
                       (test-exn "Plugin procedure is under contract"
                                 exn:fail:contract?
                                 (λ () ((get-extract-procedure "a.xz") (void))))

                       ; Confirm that the plugin read the only
                       ; available byte.
                       (define-values (i o) (make-pipe))
                       (write-byte 1 o)
                       (flush-output o)
                       (close-output-port o)
                       ((get-extract-procedure "a.xz") i)
                       (check-pred eof-object? (read-byte i)))))

                  (λ ()
                    (delete-file plugin-path))))

  
  (test-case "Can pack and unpack an archive"
    (define working-dir (make-temporary-file "tmp~a" 'directory))

    (dynamic-wind void
                  (λ ()
                    (parameterize ([current-directory working-dir])
                      (define dir/ (build-path "input-dir"))
                      (make-directory dir/)
                      (define input-a (make-file "input-dir/a"))
                      (define input-b (make-file "input-dir/sub/b"))
                      (define input-c (make-file "input-dir/very/deep/c"))
                      (define .tar "a.tar")

                      (call-with-output-file .tar
                        (λ (o)
                          (tar->output #:follow-links? #f
                                       (list input-a input-b input-c)
                                       o)))

                      (test-pred "Archive exists" file-exists? .tar)

                      (delete-directory/files dir/)

                      (run-log (extract .tar))

                      (test-file input-a)
                      (test-file input-b)
                      (test-file input-c)))
                  (λ () (delete-directory/files working-dir)))))