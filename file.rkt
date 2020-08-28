#lang racket/base

(provide (all-defined-out)
         (all-from-out racket/file))

(require racket/file
         racket/function
         racket/generator
         racket/list
         racket/path
         racket/set
         racket/sequence
         "config.rkt"
         "message.rkt"
         "path.rkt"
         "setting.rkt"
         "string.rkt"
         "workspace.rkt"
         "query.rkt")


(define+provide-message $made-symlink (target-path link-path))
(define+provide-message $deleted-file (path))


(define (delete-file* path)
  (attach-message
   (if (or (file-exists? path)
           (link-exists? path))
       (begin (delete-file path)
              path)
       #f)
   ($deleted-file path)))


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
  (make-file-or-directory-link to link-path)
  (attach-message link-path
                  ($made-symlink to link-path)))


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


(define (call-with-temporary-directory f #:cd? [cd? #t])
  (define tmp-dir (make-temporary-file "rktdir~a" 'directory))
  (dynamic-wind void
                (λ () (parameterize ([current-directory (if cd? tmp-dir (current-directory))])
                        (f tmp-dir)))
                (λ () (delete-directory/files tmp-dir))))

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
