#lang racket/base

; Extend file system operations

(provide (all-defined-out)
         (all-from-out racket/file))

(require racket/file
         racket/format
         racket/function
         racket/generator
         racket/list
         racket/path
         racket/port
         racket/set
         racket/sequence
         file/glob
         "codec.rkt"
         "message.rkt"
         "path.rkt"
         "query.rkt"
         "setting.rkt"
         "string.rkt"
         "url.rkt"
         "workspace.rkt")

(define (in-paths variant [wrt (current-directory)])
  (sequence-filter (cond [(or (regexp? variant)
                              (pregexp? variant)
                              (byte-pregexp? variant)
                              (byte-regexp? variant))
                          (curry regexp-match? variant)]
                         [(string? variant)
                          (λ (p)
                            (parameterize ([current-directory wrt])
                              (glob-match? variant p)))])
                   (in-directory wrt)))

(define (delete-file* path)
  (when (or (file-exists? path) (link-exists? path))
    (delete-file path)))


(define (in-acyclic-directory start-dir [use-dir? (λ _ #t)])
  (in-directory start-dir
                (λ (p) (if (link-exists? p)
                           (not (path-cycles? p))
                           (use-dir? p)))))


(define (in-racket-modules start-path)
  (sequence-filter (λ (p)
                     (and (not (link-exists? p))
                          (not (equal? (path->string (file-name-from-path p))
                                       CONVENTIONAL_WORKSPACE_NAME))
                          (file-exists? p)
                          (member (path-get-extension p)
                                  '(#".rkt" #".ss" #".scrbl"))))
                   (in-acyclic-directory start-path)))


(define (in-matching-files patterns start-dir)
  (in-generator
   (for ([path (in-directory start-dir (negate link-exists?))])
     (define rel-path (find-relative-path start-dir path))
     (when (and (file-exists? rel-path)
                (ormap (λ (p) (regexp-match? p rel-path)) patterns))
       (yield rel-path)))))


(define (in-workspace)
  (in-acyclic-directory (build-workspace-path)
                        (λ (p) (not (member (path->string (file-name-from-path p)) '(".git"))))))


(define (make-link/clobber to link-path)
  (make-directory* (or (path-only link-path) (current-directory)))
  (when (link-exists? link-path)
    (delete-file link-path))
  (make-file-or-directory-link to link-path))


(define (delete-directory/files/empty-parents path)
  (delete-directory/files path)
  (define cpath (path->complete-path path))
  (let loop ([current (simplify-path cpath)] [next (../ cpath)])
    (if (or (equal? current next)
            (not (directory-empty? next)))
        (void)
        (begin (delete-directory next)
               (loop next (../ next))))))


(define (directory-empty? path)
  (null? (directory-list path)))


(define (call-with-temporary-directory f #:cd? [cd? #t] #:base [base #f])
  (when base (make-directory* base))
  (define tmp-dir (make-temporary-file "rktdir~a" 'directory base))
  (dynamic-wind void
                (λ () (parameterize ([current-directory (if cd? tmp-dir (current-directory))])
                        (f tmp-dir)))
                (λ ()
                  (when (directory-exists? tmp-dir)
                    (delete-directory/files tmp-dir)))))

(define (call-with-temporary-file proc)
  (define tmp (make-temporary-file "~a"))
  (dynamic-wind void
                (λ () (proc tmp))
                (λ () (delete-file tmp))))


(define (get-cached-file name dirname make!)
  (define tmp (build-workspace-path "tmp" dirname))
  (unless (file-exists? tmp)
    (make-directory* (path-only tmp))
    (make! tmp))
  tmp)


(define (copy-limited-port-to-file! path in limit)
  (call-with-output-file path #:exists 'truncate/replace
    (λ (out)
      (copy-port (if (eq? limit +inf.0)
                     in
                     (make-limited-input-port in limit))
                 out)
      (close-input-port in))))


(define (get-cached-file* variant [limit +inf.0])
  (cond [(bytes? variant)
         (get-cached-file "bytes"
          (coerce-string (encode 'base32 (subbytes variant 0 (min 64 (bytes-length variant)))))
          (λ (path)
            (copy-limited-port-to-file! path (open-input-bytes variant) limit)))]

        [(file-exists? variant)
         variant]

        [(url-string? variant)
         (get-cached-file
          variant
          (coerce-string (encode 'base32 variant))
          (λ (path)
            (copy-limited-port-to-file! path (get-pure-port (string->url variant)) limit)))]

        [else
         (raise-user-error (format (~a "Cannot understand byte source ~v~n"
                                       "  Expected a path to a file, bytes, or URL (as a string).")
                                   variant))]))


(define-syntax-rule (with-temporary-directory body ...)
  (call-with-temporary-directory
   (λ (tmp-dir) body ...)))


(module+ test
  (provide temp-fs dir >> test-workspace)
  (require rackunit
           racket/set
           (for-syntax racket/base))


  (define-syntax-rule (test-workspace message body ...)
    (test-case message
      (call-with-temporary-directory
       #:cd? #t
       (λ (tmp-dir)
         (parameterize ([workspace-directory tmp-dir])
           body ...)))))

  (define (display-to-temp-file content)
    (define path (make-temporary-file "~a"))
    (display-to-file #:exists 'truncate/replace content path)
    path)

  (define-syntax-rule (temp-fs expr ...)
    (let ([tmpdir (make-temporary-file "~a" 'directory)])
      (parameterize ([current-directory tmpdir])
        expr ...
        (delete-directory/files tmpdir))))

  (define-syntax (dir stx)
    (syntax-case stx ()
      [(_ kw body ...)
       (with-syntax ([dirname (keyword->string (syntax-e #'kw))])
         #'(begin (make-directory dirname)
                  (parameterize ([current-directory dirname])
                    body ...)))]))

  (define-syntax (>> stx)
    (syntax-case stx ()
      [(_ kw val)
       (let ([e (syntax-e #'val)])
         (with-syntax ([fname (keyword->string (syntax-e #'kw))]
                       [writer (cond [(string? e)
                                      #'(λ (o) (displayln val o))]
                                     [(bytes? e)
                                      #'(λ (o) (write-bytes val o))]
                                     [else #'val])])
           #'(call-with-output-file fname writer)))]
      [(_ kw)
       #'(>> kw "")])))
