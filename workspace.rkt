#lang racket/base

; Define a directory used for all file I/O

(provide build-workspace-path
         find-workspace-directory
         make-workspace-path-builder
         workspace-directory
         path-in-workspace?
         CONVENTIONAL_WORKSPACE_NAME
         workspace-directory/c
         show-workspace-envvar-error?)

(require racket/contract
         racket/file
         racket/path
         "codec.rkt"
         "message.rkt")

(define CONVENTIONAL_WORKSPACE_NAME "xiden-workspace")

(define+provide-message $invalid-workspace-envvar ())

(define (find-workspace-directory [current-dir (current-directory)])
  (define complete-current-dir (simplify-path (path->complete-path current-dir)))
  (define candidate (build-path complete-current-dir CONVENTIONAL_WORKSPACE_NAME))
  (if (directory-exists? candidate)
      candidate
      (let ([parent (simplify-path (build-path complete-current-dir 'up))])
        (and (not (equal? complete-current-dir parent))
             (find-workspace-directory parent)))))

(define workspace-directory/c
  (and/c complete-path?
         (or/c directory-exists?
               (and/c (not/c file-exists?)
                      (not/c directory-exists?)
                      (not/c link-exists?)))))


(define (assert-workspace-directory v)
  (invariant-assertion workspace-directory/c v))

(define (get-initial-workspace-directory)
  (or (with-handlers
        ([exn:fail:contract? (λ (e) #f)])
        (let ([ws (getenv "XIDEN_WORKSPACE")])
          (assert-workspace-directory ws)))
      (find-workspace-directory)
      (build-path (current-directory)
                  CONVENTIONAL_WORKSPACE_NAME)))

(define (show-workspace-envvar-error?)
  (let ([ws (getenv "XIDEN_WORKSPACE")])
    (and ws
         (with-handlers ([exn:fail:contract?
                          (λ (e) (not (equal? ws (workspace-directory))))])
           (assert-workspace-directory ws)))))

(define workspace-directory
  (make-parameter (get-initial-workspace-directory)
                  assert-workspace-directory))

(define (build-workspace-path . paths)
  (apply build-path (workspace-directory)
         paths))

(define (make-workspace-path-builder base)
  (λ paths
    (define dir (build-workspace-path base))
    (make-directory* dir)
    (apply build-path dir paths)))


(define (path-in-workspace? path)
  (define simplified (simple-form-path path))
  (define rel-path
    (find-relative-path
     #:more-than-same? #f
     (path->directory-path (workspace-directory))
     simplified))

  (equal? simplified (build-workspace-path rel-path)))


(module+ test
  (require racket/file
           rackunit)


  (define (tmp p #:dir? dir?)
    (define tmpfile (make-temporary-file "~a" (and dir? 'directory)))
    (dynamic-wind void
                  (λ () (p tmpfile))
                  (λ ()
                    (if (directory-exists? tmpfile)
                        (delete-directory/files tmpfile)
                        (delete-file tmpfile)))))

  (test-equal? "Workspace always starts with conventional name"
               (path->string (file-name-from-path (workspace-directory)))
               CONVENTIONAL_WORKSPACE_NAME)

  (test-case "Can find workspace directory"
    (define wsn CONVENTIONAL_WORKSPACE_NAME)
    (define (build-complete-simple-path . args)
      (simplify-path (path->complete-path (apply build-path args))))

    (tmp #:dir? #t
         (λ (tmpdir)
           (parameterize ([current-directory tmpdir])
             (define right-here (build-complete-simple-path wsn))
             (define deeper (build-complete-simple-path "depeer/deeper" wsn))
             (define within-ws (build-complete-simple-path wsn wsn))

             (make-directory* right-here)
             (make-directory* deeper)
             (make-directory* within-ws)

             (check-equal? (find-workspace-directory) right-here)
             (check-equal? (find-workspace-directory "deeper") right-here)
             (check-equal? (find-workspace-directory wsn) within-ws)
             (check-equal? (find-workspace-directory "depeer/deeper") deeper)))))

  (test-exn "Guard against existing file paths"
            exn:fail:contract?
            (λ () (tmp workspace-directory)))

  (test-exn "Guard against garbage workspace directory paths"
            exn:fail:contract?
            (λ () (workspace-directory "jfkdjfkdsf")))

  (test-exn "Guard against links"
            exn:fail:contract?
            (λ () (tmp #:dir? #t
                       (λ (p)
                         (tmp (λ (l)
                                (delete-file l)
                                (make-file-or-directory-link p l)
                                (workspace-directory l))))))))
