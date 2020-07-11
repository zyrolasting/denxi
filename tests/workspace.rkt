#lang racket/base

(provide with-new-workspace)

(require "../file.rkt"
         "../workspace.rkt")

(define-syntax-rule (with-new-workspace body ...)
  (with-temporary-directory
    (let ([ws (build-path (current-directory) CONVENTIONAL_WORKSPACE_NAME)])
      (make-directory ws)
      (parameterize ([current-directory ws]
                     [ZCPKG_WORKSPACE ws])
        body ...))))
