#lang racket/base

(provide link-denxi-collection)

(require racket/runtime-path
         racket/file
         racket/path
         racket/pretty
         setup/link
         "firework.rkt")


(define-runtime-path source-code-runtime-path ".")
(define source-code/ (normalize-path source-code-runtime-path))

(define (link-denxi-collection)
  (links source-code/ #:name "denxi" #:user? #f)
  (printf "Linked ~a to the 'denxi' collection at installation scope~n"
          source-code/))


(module+ main
  (require racket/cmdline)
  (define in-place? #f)
  (command-line
   #:once-each
   [("-i" "--in-place")
    "Use existing source code"
    (set! in-place? #t)]
   #:args ([edition "unnamed"])
   (define dirname
     (format "denxi-~a-~a" edition (current-seconds)))
   (if in-place?
       (link-denxi-collection)
       (pretty-write #:newline? #t
                     `(module setup racket/base
                        (require racket/runtime-path)
                        (define-runtime-path extraction/ ,dirname)
                        ,(firework dirname
                                   (for/hash ([path (in-directory source-code/)]
                                              #:when (and (not (regexp-match? #px"\\.git" path))
                                                          (file-exists? path)))
                                     (values (find-relative-path source-code/ path)
                                             (cons #o440 (file->bytes path)))))
                        (require (submod 'packed main))
                        ((dynamic-require (build-path extraction/ "setup.rkt") 'link)))))))
