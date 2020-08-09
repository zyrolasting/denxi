#lang racket/base

(provide build-workspace-path
         find-workspace-directory
         workspace-directory
         CONVENTIONAL_WORKSPACE_NAME
         CONVENTIONAL_DEPENDENCY_DIRECTORY_NAME
         CONVENTIONAL_PACKAGE_INFO_FILE_NAME
         CONVENTIONAL_LAUNCHER_AUX_DIRECTORY_NAME)

(require racket/contract
         racket/path)

(define CONVENTIONAL_WORKSPACE_NAME "zcpkg-workspace")
(define CONVENTIONAL_DEPENDENCY_DIRECTORY_NAME "zcpkg-deps")
(define CONVENTIONAL_PACKAGE_INFO_FILE_NAME "zcpkg.rkt")
(define CONVENTIONAL_LAUNCHER_AUX_DIRECTORY_NAME "launcher-aux")

(define (find-workspace-directory [current-dir (current-directory)])
  (define complete-current-dir (simplify-path (path->complete-path current-dir)))
  (define candidate (build-path complete-current-dir CONVENTIONAL_WORKSPACE_NAME))
  (if (directory-exists? candidate)
      candidate
      (let ([parent (simplify-path (build-path complete-current-dir 'up))])
        (and (not (equal? complete-current-dir parent))
             (find-workspace-directory parent)))))

(define workspace-directory
  (make-parameter
   (or (find-workspace-directory)
       (build-path (current-directory)
                   CONVENTIONAL_WORKSPACE_NAME))
   (λ (v)
     (invariant-assertion
      (and/c complete-path?
             (or/c directory-exists?
                   (and/c (not/c file-exists?)
                          (not/c directory-exists?)
                          (not/c link-exists?))))
      v))))

(define (build-workspace-path . paths)
  (apply build-path (workspace-directory)
         paths))

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

  (test-not-exn "Conventional names are not reserved file names"
                (λ ()
                  (tmp #:dir? #t
                       (λ (tmpdir)
                         (parameterize ([current-directory tmpdir])
                           (dynamic-wind
                             void
                             (λ ()
                               (make-directory CONVENTIONAL_WORKSPACE_NAME)
                               (make-directory CONVENTIONAL_DEPENDENCY_DIRECTORY_NAME)
                               (display-to-file "" CONVENTIONAL_PACKAGE_INFO_FILE_NAME))
                             (λ ()
                               (when (directory-exists? CONVENTIONAL_WORKSPACE_NAME)
                                 (delete-directory CONVENTIONAL_WORKSPACE_NAME))
                               (when (directory-exists? CONVENTIONAL_DEPENDENCY_DIRECTORY_NAME)
                                 (delete-directory CONVENTIONAL_DEPENDENCY_DIRECTORY_NAME))
                               (when (file-exists? CONVENTIONAL_PACKAGE_INFO_FILE_NAME)
                                 (delete-file CONVENTIONAL_PACKAGE_INFO_FILE_NAME)))))))))

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
