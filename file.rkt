#lang racket/base

; Extend file system operations

(provide (all-defined-out)
         (all-from-out racket/file))

(require file/glob
         racket/file
         racket/format
         racket/function
         racket/generator
         racket/port
         racket/sequence
         "codec.rkt"
         "logged.rkt"
         "message.rkt"
         "path.rkt"
         "url.rkt"
         "workspace.rkt")

(define+provide-message $path-not-found (pattern wrt))

; Use module-level cache because filesystem-root-list may take a while
; on Windows.
(define filesystem-root-list/cached
  (let ([cache #f])
    (λ ()
      (unless cache
        (set! cache (filesystem-root-list)))
      cache)))


(define-logged (path-matching variant [wrt (current-directory)])
  (with-handlers ([exn? (λ (e) ($fail ($path-not-found variant wrt)))])
    (sequence-ref (in-paths variant wrt) 0)))

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



(define (in-matching-files patterns start-dir)
  (in-generator
   (for ([path (in-directory start-dir (negate link-exists?))])
     (define rel-path (find-relative-path start-dir path))
     (when (and (file-exists? rel-path)
                (ormap (λ (p) (regexp-match? p rel-path)) patterns))
       (yield rel-path)))))


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
