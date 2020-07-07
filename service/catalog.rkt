#lang racket/base

(provide make-catalog-dispatcher)

(require racket/runtime-path
         net/url
         web-server/http
         web-server/web-server
         web-server/dispatchers/filesystem-map
         (prefix-in files:
                    web-server/dispatchers/dispatch-files)
         "../workspace.rkt")

(define (make-catalog-dispatcher install-path)
  (files:make #:url->path (make-url->path install-path)))
