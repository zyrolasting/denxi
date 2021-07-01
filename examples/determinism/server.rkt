#lang racket/base

(require web-server/web-server
         web-server/http
         web-server/dispatchers/dispatch-lift)

(module+ main
  (define stop
    (serve #:port 9147
           #:dispatch
           (make
            (λ _
              (if (zero? (random 0 5))
                  (response/output (λ (o) (write-bytes #"abc" o)))
                  (response/empty #:code 500))))))
  (with-handlers ([values (λ _ (stop))])
    (sync/enable-break never-evt)))
