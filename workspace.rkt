#lang racket/base

(provide build-workspace-path
         find-workspace-directory
         ZCPKG_WORKSPACE
         CONVENTIONAL_WORKSPACE_NAME
         CONVENTIONAL_DEPENDENCY_DIRECTORY_NAME
         ws/)

(require idiocket/contract
         idiocket/path
         "setting.rkt")

(define CONVENTIONAL_WORKSPACE_NAME "zcw")
(define CONVENTIONAL_DEPENDENCY_DIRECTORY_NAME "zcdeps")

(define (find-workspace-directory [current-dir (current-directory)])
  (define complete-current-dir (simplify-path (path->complete-path current-dir)))
  (define candidate (build-path complete-current-dir CONVENTIONAL_WORKSPACE_NAME))
  (if (directory-exists? candidate)
      candidate
      (let ([parent (simplify-path (build-path complete-current-dir 'up))])
        (and (not (equal? complete-current-dir parent))
             (find-workspace-directory parent)))))

(define ZCPKG_WORKSPACE
  (make-setting 'ZCPKG_WORKSPACE
                (and/c complete-path?
                       (or/c directory-exists?
                             (and/c (not/c file-exists?)
                                    (not/c directory-exists?)
                                    (not/c link-exists?))))
                (find-workspace-directory)))

(define (build-workspace-path . paths)
  (apply build-path (ZCPKG_WORKSPACE)
         paths))

(define ws/ build-workspace-path)
