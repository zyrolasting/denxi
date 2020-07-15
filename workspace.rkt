#lang racket/base

(provide build-workspace-path
         find-workspace-directory
         workspace-directory
         CONVENTIONAL_WORKSPACE_NAME
         CONVENTIONAL_DEPENDENCY_DIRECTORY_NAME
         CONVENTIONAL_PACKAGE_INFO_DIRECTORY_NAME)

(require idiocket/contract
         idiocket/path
         "setting.rkt")

(define CONVENTIONAL_WORKSPACE_NAME "zcpkg-workspace")
(define CONVENTIONAL_DEPENDENCY_DIRECTORY_NAME "zcpkg-deps")
(define CONVENTIONAL_PACKAGE_INFO_DIRECTORY_NAME "zcpkg-info")

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
