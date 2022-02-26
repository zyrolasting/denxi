(module setup racket/base
  (require setup/link racket/runtime-path)
  (define-runtime-path source-code/ ".")
  (module+ main
    (require racket/path)
    (void (links (simple-form-path source-code/) #:name "denxi" #:user? #f))
    (printf "Linked ~a to the 'denxi' collection at installation scope~n"
            source-code/)))
