#lang racket/base

; Extend file system operations

(require racket/contract)
(provide (all-defined-out)
         (all-from-out racket/file))

(require file/glob
         racket/file
         racket/function
         racket/generator
         racket/sequence
         "message.rkt"
         "path.rkt"
         "subprogram.rkt")

(define+provide-message $path-not-found (pattern wrt))

; Use module-level cache because filesystem-root-list may take a while
; on Windows.
(define filesystem-root-list/cached
  (let ([cache #f])
    (λ ()
      (unless cache
        (set! cache (filesystem-root-list)))
      cache)))


(define-subprogram (path-matching variant [wrt (current-directory)])
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


(define (something-exists? path)
  (or (file-exists? path)
      (directory-exists? path)
      (link-exists? path)))


(define (linked? link-path path)
  (and (link-exists? link-path)
       (something-exists? path)
       (equal? (file-or-directory-identity link-path)
               (file-or-directory-identity path))))


(define (file-link-exists? path)
  (and (link-exists? path)
       (file-exists? path)))


(define (call-with-temporary-directory f #:cd? [cd? #t] #:base [base #f])
  (let ([path #f])
    (dynamic-wind (λ ()
                    (when base (make-directory* base))
                    (set! path (make-temporary-file "rktdir~a" 'directory base)))
                  (λ ()
                    (parameterize ([current-directory (if cd? path (current-directory))])
                      (f path)))
                  (λ ()
                    (when (and path (directory-exists? path))
                      (delete-directory/files path))))))


(define (call-with-temporary-file proc)
  (let ([path #f])
    (dynamic-wind (λ ()
                    (set! path (make-temporary-file "~a")))
                  (λ ()
                    (proc path))
                  (λ ()
                    (delete-file path)))))


(define-syntax-rule (with-temporary-directory body ...)
  (call-with-temporary-directory (λ _ body ...)))
