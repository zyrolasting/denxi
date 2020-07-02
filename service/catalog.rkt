#lang racket/base

(provide catalog-dispatcher)

(require racket/runtime-path
         net/url
         web-server/http
         web-server/web-server
         web-server/dispatchers/filesystem-map
         (prefix-in files:
                    web-server/dispatchers/dispatch-files)
         "../workspace.rkt")

(define catalog-dispatcher
  (files:make #:url->path (make-url->path (build-workspace-path "home"))))
