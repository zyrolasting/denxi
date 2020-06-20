#lang racket/base

(module reader racket/base
  (require racket/path)
  (provide (rename-out [-read read]
                       [-read-syntax read-syntax]))
  (define (-read in)
    (-read-syntax #f in))
  (define (-read-syntax src in)
    (with-syntax ([path (path->string (file-name-from-path src))])
      #'(module content racket/base
          (provide data)
          (define data path)))))
