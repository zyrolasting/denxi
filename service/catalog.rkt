#lang racket/base

(provide catalog-dispatcher)

(require racket/runtime-path
         net/url
         web-server/http
         web-server/web-server
         web-server/dispatchers/filesystem-map
         (prefix-in files:
                    web-server/dispatchers/dispatch-files)
         "path.rkt")

(define url->path
  (make-url->path catalogs/))

(define catalog-dispatcher
  (files:make #:url->path url->path))
