#lang racket/base

; Extract archives

(require file/tar
         file/untar
         file/untgz
         file/unzip
         racket/contract
         racket/file
         racket/format
         racket/match
         racket/path
         racket/string
         "input.rkt"
         "subprogram.rkt"
         "message.rkt"
         "monad.rkt")

(provide
 (contract-out
  [extract (-> (or/c path-string? input-port?) (subprogram/c void?))]
  [extract-input (->* (string?) (#:keep? any/c) subprogram?)]
  [current-find-extract-procedure
   (-> path-string? (or/c #f (-> input-port? any)))]))

(define current-find-extract-procedure
  (make-parameter (λ _ #f)))

(define+provide-message $extract-report (status target))

(define-subprogram (extract variant)
  (let start ([in variant])
    (if (path-string? in)
        (call-with-input-file in start)
        (let* ([path (object-name in)]
               [proc (get-extract-procedure path)])
          (if proc
              ($attach (void (proc in)) ($extract-report 'done path))
              ($fail ($extract-report 'unsupported path)))))))


(define (extract-input #:keep? [keep? #f] name)
  (if keep?
      (mdo p := (keep-input name)
           (extract p))
      (mdo i := (input-ref name)
           p := (resolve-input i)
           (extract p)
           (release-input i))))

(define (get-extract-procedure name)
  (match (reverse (string-split (~a (file-name-from-path name)) "."))
    [(list "tar" _ ...)
     untar]
    [(or (list "tgz" _ ...) (list "gz" "tar" _ ...))
     untgz]
    [(list "zip" _ ...)
     unzip]
    [else
     ((current-find-extract-procedure) name)]))


(module+ test
  (require rackunit
           racket/port
           "artifact.rkt"
           "integrity.rkt"
           "source.rkt"
           (submod "state.rkt" test)
           (submod "subprogram.rkt" test))

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

    (parameterize ([current-find-extract-procedure
                    (λ (name) (λ (in) (void (read-byte in))))])
      ; Confirm that we read the only available byte.
      (define-values (i o) (make-pipe))
      (write-byte 1 o)
      (flush-output o)
      (close-output-port o)
      ((get-extract-procedure "a.xz") i)
      (check-pred eof-object? (read-byte i))))


  (test-workspace "Can pack and unpack an archive"
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

    ; We don't want to protect against use of this dummy data.
    (XIDEN_TRUST_BAD_DIGEST #t
     (λ ()
       (define faux-archive
         (package-input "archive.tgz"
                        (make-artifact (file-source .tar))))

       (parameterize ([current-inputs (list faux-archive)]
                      [current-output-port (open-output-nowhere)])
         (check-subprogram
          (extract-input (package-input-name faux-archive))
          (λ (result messages)
            (check-false (link-exists? (package-input-name faux-archive)))
            (test-file input-a)
            (test-file input-b)
            (test-file input-c)))

         (delete-file input-a)
         (delete-file input-b)
         (delete-file input-c)

         (call-with-snake-oil-chf-trust
          (λ ()
            (check-subprogram
             (extract-input #:keep? #t
                            (package-input-name faux-archive))
             (λ (result messages)
               (check-true (link-exists? (package-input-name faux-archive)))
               (test-file input-a)
               (test-file input-b)
               (test-file input-c))))))))))
